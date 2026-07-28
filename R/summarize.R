utils::globalVariables(c("n_reps"))

#' Bind a raw per-replicate metric list into one tidy long table
#'
#' The raw metric producers ([nrv_metrics_landscape()], [calculatePatchMetrics()],
#' [calculatePatchMetricsSeral()]) each return a named `list` -- one element per
#' entry in `funList` -- of long `data.frame`s that already share the columns
#' `level`, `class`, `metric`, `value`, `rep`, `time`, `poly` (the
#' \pkg{landscapemetrics} `metric` name together with `level` uniquely identifies
#' each metric, so the `funList` element name is not carried as a separate column).
#' This binds them into a single long table ready for [write_nrv_parquet()],
#' optionally stamping run-level identifiers so a union of several runs can be
#' grouped by them in [summarize_nrv()].
#'
#' @param metric_list A named `list` of raw metric `data.frame`s, or a single
#'   `data.frame`. `NULL`/empty elements are dropped; an all-empty input returns a
#'   zero-row `data.frame`.
#' @param studyArea,scenario Optional length-1 identifiers prepended as columns
#'   (when not `NULL`).
#'
#' @return A long `data.frame` (the row-bind of `metric_list`), with `studyArea`
#'   and/or `scenario` prepended when supplied.
#'
#' @export
tidy_nrv_metrics <- function(metric_list, studyArea = NULL, scenario = NULL) {
  if (is.data.frame(metric_list)) {
    metric_list <- list(metric_list)
  }
  metric_list <- metric_list[!vapply(metric_list, is.null, logical(1))]
  metric_list <- metric_list[vapply(metric_list, function(d) nrow(d) > 0L, logical(1))]
  if (length(metric_list) == 0L) {
    return(data.frame())
  }
  d <- as.data.frame(
    data.table::rbindlist(metric_list, use.names = TRUE, fill = TRUE),
    stringsAsFactors = FALSE
  )
  ids <- list(studyArea = studyArea, scenario = scenario)
  ids <- ids[!vapply(ids, is.null, logical(1))]
  if (length(ids)) {
    d <- data.frame(ids, d, row.names = NULL, stringsAsFactors = FALSE, check.names = FALSE)
  }
  d
}

#' Ordered seral stage classes for BC forests
#'
#' Returns the canonical BC seral-stage class labels in display order (early ->
#' old, with the NDT4 Fir/Pine/Other groups), for use as `levels` when ordering a
#' `class` column produced by [calculatePatchMetricsSeral()] /
#' [seralStageMapGeneratorBC()], e.g. `factor(class, levels = seral_stages())`.
#'
#' @return An ordered character vector of seral-stage class labels.
#'
#' @export
seral_stages <- function() {
  .seralStagesBC
}

#' Write one replicate's metric table to a partitioned parquet
#'
#' Writes `df` (one replicate's tidy long metric table) under
#' `root/replicate=<replicate>/part-0.parquet`, stamping `replicate` as a data
#' column. The write is atomic on POSIX/NFS: the parquet is written to a unique
#' temporary name in the destination directory and renamed into place, so a
#' concurrent reader (or a retried write) never observes a partial file. Many
#' replicate writers can therefore run concurrently against an NFS output
#' directory without contention (each writes its own `replicate=` partition).
#'
#' `root` is caller-provided and is normally the per-study-area output location
#' (e.g. `<scenario>/<studyArea>/_aggregates/<name>`); this function is
#' study-area agnostic. Run-level identifiers (`studyArea`, `scenario`, ...)
#' should already be columns of `df` so that [summarize_nrv()] can group by them
#' when a union of roots is opened together.
#'
#' @param df A `data.frame` of one replicate's metrics. `NULL` or a zero-row
#'   input returns `NULL` (nothing is written).
#' @param root Directory under which the `replicate=<replicate>` partition is
#'   created (created recursively if needed).
#' @param replicate Replicate identifier (integer or string) used for the
#'   partition directory and stamped as the `replicate` column.
#'
#' @return The written parquet path (invisibly returned as a length-1 character),
#'   or `NULL` for empty input.
#'
#' @export
write_nrv_parquet <- function(df, root, replicate) {
  if (is.null(df) || nrow(df) == 0L) {
    return(NULL)
  }
  df[["replicate"]] <- replicate
  dst_dir <- file.path(root, paste0("replicate=", replicate))
  fs::dir_create(dst_dir)
  dst <- file.path(dst_dir, "part-0.parquet")
  tmp <- tempfile(tmpdir = dst_dir, fileext = ".parquet.tmp")
  arrow::write_parquet(df, tmp)
  if (!file.rename(tmp, dst)) {
    unlink(tmp)
    stop("write_nrv_parquet(): failed to publish ", dst, call. = FALSE)
  }
  dst
}

#' Open replicate parquet(s) as one lazy Arrow dataset
#'
#' Resolves `x` -- parquet file paths and/or dataset roots -- to a flat list of
#' `*.parquet` files and opens them as a single lazy [arrow::open_dataset()]
#' `Dataset`. Because `replicate` and any run-level identifiers are stored as
#' data columns (not inferred from the directory tree), files from several roots
#' (multiple study areas or scenarios) open together without a `UnionDataset`,
#' and opening from explicit file paths avoids depending on NFS directory-listing
#' freshness.
#'
#' @param x Character vector of parquet file paths and/or directories; directories
#'   are scanned recursively for `*.parquet`.
#'
#' @return An Arrow `Dataset` (lazy), or `NULL` if no parquet files are found.
#'
#' @export
open_nrv_dataset <- function(x) {
  x <- x[nzchar(x)]
  if (length(x) == 0L) {
    return(NULL)
  }
  is_dir <- dir.exists(x)
  files <- c(
    x[!is_dir & grepl("\\.parquet$", x)],
    if (any(is_dir)) {
      list.files(x[is_dir], pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
    }
  )
  files <- files[file.exists(files)]
  if (length(files) == 0L) {
    return(NULL)
  }
  arrow::open_dataset(files)
}

#' Summarize replicate metrics into a range-of-variation envelope
#'
#' Opens the replicate parquet(s) lazily and pushes the across-replicate
#' reduction down to Arrow compute, so only the small envelope is materialized in
#' R -- the per-replicate rows never come back. For each combination of the
#' identifier columns it returns the replicate count and the mean, standard
#' deviation, min, max and median of `value_col`, plus the standard error and 95%
#' confidence half-width computed in R from the collected envelope.
#'
#' Note: Arrow computes *approximate* medians and quartiles; this is acceptable
#' for range-of-variation envelopes. `min`/`max`/`mean`/`sd`/`n_reps` are exact.
#' The five-number summary (`min`, `q25`, `median`, `q75`, `max`) supports
#' box-and-whisker range-of-variation plots ([plot_nrv_envelope()] with
#' `type = "boxplot"`).
#'
#' @param x Parquet paths / roots (see [open_nrv_dataset()]) or an Arrow
#'   `Dataset` / query.
#' @param value_col Name of the value column to aggregate (default `"value"`).
#' @param id_cols Grouping columns; `NULL` (default) auto-detects those present
#'   among `studyArea`, `scenario`, `rptPoly`, `time`, `poly`, `class`, `level`,
#'   `metric`, `metric.1`. (`rptPoly` names the reporting POLYGON LAYER a metric
#'   was summarised over -- e.g. "BEC zones" vs "Landscape Units" -- where `poly`
#'   is the individual subregion within that layer.)
#'
#' @return A `data.frame` with the id columns plus `n_reps`, `mean`, `sd`, `min`,
#'   `q25`, `median`, `q75`, `max`, `se`, `ci`; zero rows if there is no data.
#'
#' @export
summarize_nrv <- function(x, value_col = "value", id_cols = NULL) {
  ds <- if (inherits(x, c("Dataset", "arrow_dplyr_query"))) x else open_nrv_dataset(x)
  if (is.null(ds)) {
    return(data.frame())
  }
  cols <- names(ds)
  if (!value_col %in% cols) {
    stop(
      "summarize_nrv(): value column '",
      value_col,
      "' not in dataset (have: ",
      paste(cols, collapse = ", "),
      ")",
      call. = FALSE
    )
  }
  if (is.null(id_cols)) {
    id_cols <- intersect(
      c("studyArea", "scenario", "rptPoly", "time", "poly", "class", "level", "metric", "metric.1"),
      cols
    )
  }
  id_cols <- setdiff(id_cols, value_col)
  vsym <- rlang::sym(value_col)
  ## Arrow's median/quantile are approximate (documented); muffle only those expected notes.
  ## Arrow emits them while BUILDING the query, so wrap the whole pipeline, not just collect().
  out <- withCallingHandlers(
    ds |>
      dplyr::group_by(!!!rlang::syms(id_cols)) |>
      dplyr::summarise(
        n_reps = dplyr::n(),
        mean = mean(!!vsym, na.rm = TRUE),
        sd = stats::sd(!!vsym, na.rm = TRUE),
        min = min(!!vsym, na.rm = TRUE),
        q25 = stats::quantile(!!vsym, 0.25, na.rm = TRUE),
        median = stats::median(!!vsym, na.rm = TRUE),
        q75 = stats::quantile(!!vsym, 0.75, na.rm = TRUE),
        max = max(!!vsym, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::collect(),
    warning = function(w) {
      if (grepl("approximate (median|quantile)", conditionMessage(w))) {
        invokeRestart("muffleWarning")
      }
    }
  )
  out <- as.data.frame(out)
  out$se <- out$sd / sqrt(out$n_reps)
  out$ci <- out$se * stats::qt(0.975, pmax(out$n_reps - 1L, 1L))
  out
}
