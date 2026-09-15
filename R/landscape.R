utils::globalVariables(c(
  "poly", "time"
))

#' Calculate landscape metrics (raw, per replicate)
#'
#' Computes the requested landscape-level \pkg{landscapemetrics} for each
#' vegetation-type map in `vtm` within each summary polygon, returning the *raw*
#' per-replicate values (one row per replicate x time x polygon x metric). No
#' across-replicate reduction is performed here: pass the result (via
#' [tidy_nrv_metrics()] / [write_nrv_parquet()]) to [summarize_nrv()] to build the
#' range-of-variation envelope. Keeping the producer raw is what makes the summary
#' path memory-bounded -- each replicate's raw table can be written to its own
#' parquet partition and the reduction pushed down to Arrow compute, instead of
#' binding every replicate into memory before summarising.
#'
#' `vtm` file paths encode the replicate/time/polygon the way the summary path
#' expects: the parent directory is the replicate (`rep<NN>`) and the file name is
#' `<prefix>_year<YYYY>.tif`.
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template vtm
#'
#' @template funList
#'
#' @return a named `list` (one element per entry in `funList`), each a raw long
#'   `data.frame` with columns `layer`, `level`, `class`, `id`, `metric`, `value`,
#'   `rep`, `time`, `poly`.
#'
#' @export
nrv_metrics_landscape <- function(summaryPolys, polyCol, vtm, funList = NULL) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  polyNames <- unique(summaryPolys[[polyCol]])

  if (is.null(funList)) {
    funList <- default_landscape_metrics()
  }
  names(funList) <- funList

  fragStats <- future.apply::future_lapply(
    vtm,
    function(f) {
      r <- terra::rast(f)
      byPoly <- lapply(polyNames, function(polyName) {
        subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]
        rc <- terra::crop(r, subpoly)
        rcm <- terra::mask(rc, subpoly)

        ## skip empty subregions (no forested pixels after crop/mask): the landscape-level lsm_l_*
        ## metrics error on an all-NA raster. Return an empty table per metric so the subregion still
        ## appears (with no rows) in output. Mirrors the patchStats() / patchStatsSeral() guard.
        if (terra::global(rcm, "notNA")[[1L]] == 0) {
          empty <- data.frame(
            layer = integer(0),
            level = character(0),
            class = character(0),
            id = integer(0),
            metric = character(0),
            value = numeric(0)
          )
          return(stats::setNames(replicate(length(funList), empty, simplify = FALSE), funList))
        }

        out <- lapply(funList, function(fun) {
          fn <- .get_fun(fun)

          fn(rcm)
        })
        names(out) <- funList
        out
      })
      names(byPoly) <- paste(tools::file_path_sans_ext(basename(f)), polyNames, sep = "_") ## vegTypeMap_yearXXXX_polyName

      byPoly
    },
    future.packages = c("landscapemetrics", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )
  names(fragStats) <- basename(dirname(vtm)) ## repXX

  fragStats <- purrr::transpose(lapply(fragStats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(fragStats)))

  frag_stat_df <- lapply(fragStats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    lbl <- .parse_metric_labels(names(x))

    ## Stamp rep/time/poly PER ELEMENT rather than onto the row-bound table: an empty subregion (no
    ## forested pixels after crop/mask) contributes zero rows, so binding first and then mutating with
    ## the full-length label vectors would silently shift every subsequent row's rep/time/poly.
    out <- do.call(
      rbind,
      lapply(seq_along(x), function(i) {
        if (nrow(x[[i]]) == 0L) {
          return(NULL)
        }
        dplyr::mutate(x[[i]], rep = lbl$rep[i], time = lbl$time[i], poly = lbl$poly[i])
      })
    )
    if (is.null(out)) {
      out <- data.frame(
        layer = integer(0),
        level = character(0),
        class = character(0),
        id = integer(0),
        metric = character(0),
        value = numeric(0),
        rep = integer(0),
        time = integer(0),
        poly = character(0)
      )
    }
    out
  })
  names(frag_stat_df) <- funList

  return(frag_stat_df)
}
