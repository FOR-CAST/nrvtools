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

## `x` as a table that dplyr verbs run on: a data.frame or an Arrow object as given, else the
## parquet dataset that its paths / roots name (NULL when there is none).
.nrv_table <- function(x) {
  if (is.data.frame(x) || inherits(x, c("Dataset", "arrow_dplyr_query", "ArrowTabular"))) {
    return(x)
  }
  open_nrv_dataset(x)
}

## Arrow keeps NaN as a value, where R's `mean(na.rm = TRUE)` drops it with NA, so on a parquet
## dataset a single NaN made its whole group's mean NaN. Mapping NaN to NA first makes both paths
## drop it. The value column is read as double either way.
.nan_to_na <- function(ds, value_col) {
  vsym <- rlang::sym(value_col)
  dplyr::mutate(ds, !!vsym := dplyr::if_else(is.nan(!!vsym), NA_real_, as.numeric(!!vsym)))
}

## `d` sorted by `cols`. Arrow returns groups in no fixed order, so without this two runs over the
## same data differ in row order, and so do figures drawn from them. Radix order sorts strings by
## their bytes, the same in every locale.
.sort_rows <- function(d, cols) {
  cols <- intersect(cols, names(d))
  if (nrow(d) > 1L && length(cols)) {
    d <- d[do.call(order, c(unname(as.list(d[cols])), method = "radix")), , drop = FALSE]
  }
  rownames(d) <- NULL
  d
}

## Type 7 quantiles, the default of stats::quantile(), of the distinct values `v` occurring `w`
## times each: the same order statistics and interpolation, without expanding the counts.
.quantile7_counts <- function(v, w, probs) {
  o <- order(v)
  v <- v[o]
  cw <- cumsum(w[o])
  index <- 1 + (cw[[length(cw)]] - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  qs <- v[findInterval(lo - 1, cw) + 1L]
  qhi <- v[findInterval(hi - 1, cw) + 1L]
  h <- index - lo
  i <- which(index > lo & qhi != qs)
  qs[i] <- (1 - h[i]) * qs[i] + h[i] * qhi[i]
  qs
}

## The envelope statistics of the values `v` that occur `w` times each; `v` is NA for the rows
## without a value, which count in `n_reps` only. Computed from the counts alone, so they do not
## depend on the order in which Arrow's threads add up a group, as Arrow's own means and standard
## deviations do (repeated runs differed in their last digits).
.nrv_stats <- function(v, w) {
  n_reps <- sum(w)
  ok <- !is.na(v)
  if (!any(ok)) {
    return(list(
      n_reps = n_reps,
      mean = NA_real_,
      sd = NA_real_,
      min = NA_real_,
      q25 = NA_real_,
      median = NA_real_,
      q75 = NA_real_,
      max = NA_real_
    ))
  }
  o <- order(v[ok])
  v <- v[ok][o]
  w <- w[ok][o]
  n <- sum(w)
  m <- sum(v * w) / n
  q <- .quantile7_counts(v, w, c(0.25, 0.5, 0.75))
  list(
    n_reps = n_reps,
    mean = m,
    sd = if (n > 1L) sqrt(sum(w * (v - m)^2) / (n - 1L)) else NA_real_,
    min = v[[1L]],
    q25 = q[[1L]],
    median = q[[2L]],
    q75 = q[[3L]],
    max = v[[length(v)]]
  )
}

#' Summarize replicate metrics into a range-of-variation envelope
#'
#' Opens the replicate parquet(s) lazily and pushes the across-replicate
#' reduction down to Arrow compute, so only a small table is materialized in R --
#' the per-replicate rows never come back. For each combination of the
#' identifier columns it returns the replicate count and the mean, standard
#' deviation, min, quartiles, median and max of `value_col`, plus the standard
#' error and 95% confidence half-width.
#'
#' Arrow counts how often each value occurs in each group, and the statistics
#' are computed from those counts in R. The quartiles and median are therefore
#' exact: type 7 quantiles, the default of [stats::quantile()], where Arrow's own
#' are approximate (they differed by up to 15% on a replicate envelope). The other
#' statistics do not depend on the order in which Arrow's threads add up a group,
#' so a repeated run gives identical numbers. What comes back to R is one row per
#' distinct value of a group: at most the replicate count for per-replicate
#' values, and few for per-patch areas, which are whole multiples of the cell
#' area. The five-number summary (`min`, `q25`, `median`, `q75`, `max`) supports
#' box-and-whisker range-of-variation plots ([plot_nrv_envelope()] with
#' `type = "boxplot"`).
#'
#' `NaN` values are treated as missing, like `NA`. A group without a value
#' gets `NA` statistics. Rows are sorted by the id columns.
#'
#' @param x Parquet paths / roots (see [open_nrv_dataset()]), an Arrow
#'   `Dataset` / `Table` / query, or a `data.frame`, such as the per-replicate
#'   values of [nrv_replicate_values()].
#' @param value_col Name of the value column to aggregate (default `"value"`).
#' @param id_cols Grouping columns; `NULL` (default) auto-detects those present
#'   among `studyArea`, `scenario`, `rptPoly`, `time`, `poly`, `class`, `level`,
#'   `metric`, `metric.1`. (`rptPoly` names the reporting POLYGON LAYER a metric
#'   was summarised over -- e.g. "BEC zones" vs "Landscape Units" -- where `poly`
#'   is the individual subregion within that layer.)
#'
#' @return A `data.frame` with the id columns plus `n_reps` (the number of rows
#'   in the group), `mean`, `sd`, `min`, `q25`, `median`, `q75`, `max`, `se`,
#'   `ci`; zero rows if there is no data.
#'
#' @export
summarize_nrv <- function(x, value_col = "value", id_cols = NULL) {
  ds <- .nrv_table(x)
  if (is.null(ds) || (is.data.frame(ds) && !ncol(ds))) {
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
  counts <- .nan_to_na(ds, value_col) |>
    dplyr::group_by(!!!rlang::syms(c(id_cols, value_col))) |>
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") |>
    dplyr::collect() |>
    data.table::as.data.table()
  stat_cols <- c("mean", "sd", "min", "q25", "median", "q75", "max")
  if (nrow(counts)) {
    by_cols <- if (length(id_cols)) id_cols else NULL
    out <- counts[, .nrv_stats(.SD[[1L]], .SD[[2L]]), by = by_cols, .SDcols = c(value_col, ".n")]
    out <- as.data.frame(out)
  } else {
    out <- as.data.frame(counts)[0L, id_cols, drop = FALSE]
    out$n_reps <- integer(0)
    out[stat_cols] <- list(numeric(0))
  }
  out[stat_cols] <- lapply(out[stat_cols], function(v) replace(v, is.nan(v), NA_real_))
  out$se <- out$sd / sqrt(out$n_reps)
  out$ci <- out$se * stats::qt(0.975, pmax(out$n_reps - 1L, 1L))
  .sort_rows(out, id_cols)
}

#' Collapse raw metrics to one value per replicate and summary year
#'
#' Returns the values whose distribution [plot_by_class()] draws: one per
#' replicate, summary year and unit. Class- and landscape-level metrics already
#' hold one value per replicate, summary year and unit, and pass through
#' unchanged. A patch-level metric holds one value per patch, and becomes the
#' mean over that replicate's patches. On a parquet dataset the reduction is
#' pushed down to Arrow, so the per-patch rows are never collected into R.
#'
#' `NaN` values are treated as missing, like `NA`, on a parquet dataset as on a
#' `data.frame`, and a value that no row defines is `NA`. Rows are sorted by the
#' id columns, so the result does not depend on the order Arrow returns groups
#' in.
#'
#' @param x Raw per-replicate values: parquet paths / roots (see
#'   [open_nrv_dataset()]), an Arrow `Dataset` / `Table` / query, or a
#'   `data.frame`.
#' @param value_col Name of the value column (default `"value"`).
#' @param id_cols Grouping columns; `NULL` (default) uses those present among
#'   `studyArea`, `scenario`, `rptPoly`, `replicate`, `rep`, `time`, `poly`,
#'   `class`, `level`, `metric`, `metric.1`.
#'
#' @return A `data.frame` with the `id_cols`, `n` (the number of rows each value
#'   collapses: the patch count for a patch-level metric, otherwise 1) and the
#'   per-replicate mean in `value_col`; zero rows if there is no data.
#'
#' @export
#' @seealso [summarize_nrv()] for the across-replicate envelope, which takes
#'   this function's result as its input.
nrv_replicate_values <- function(x, value_col = "value", id_cols = NULL) {
  ds <- .nrv_table(x)
  if (is.null(ds)) {
    return(data.frame())
  }
  cols <- names(ds)
  if (!value_col %in% cols) {
    stop(
      "nrv_replicate_values(): value column '",
      value_col,
      "' not in data (have: ",
      paste(cols, collapse = ", "),
      ")",
      call. = FALSE
    )
  }
  if (is.null(id_cols)) {
    id_cols <- intersect(
      c(
        "studyArea",
        "scenario",
        "rptPoly",
        "replicate",
        "rep",
        "time",
        "poly",
        "class",
        "level",
        "metric",
        "metric.1"
      ),
      cols
    )
  }
  id_cols <- setdiff(id_cols, value_col)
  vsym <- rlang::sym(value_col)
  out <- .nan_to_na(ds, value_col) |>
    dplyr::group_by(!!!rlang::syms(id_cols)) |>
    dplyr::summarise(n = dplyr::n(), .value = mean(!!vsym, na.rm = TRUE), .groups = "drop") |>
    dplyr::collect()
  out <- as.data.frame(out)
  out[[".value"]][is.nan(out[[".value"]])] <- NA_real_
  names(out)[names(out) == ".value"] <- value_col
  .sort_rows(out, id_cols)
}
