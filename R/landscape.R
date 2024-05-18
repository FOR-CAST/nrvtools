utils::globalVariables(c(
  "N", "poly", "sd", "se", "time", "value"
))

#' Calculate landscape metrics
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template vtm
#'
#' @param funList list of character strings specifying function names used to calculate summary
#'                landscape metrics.
#'
#' @return summary `data.frame` object
#'
#' @export
calculateLandscapeMetrics <- function(summaryPolys, polyCol, vtm, funList = NULL) {
  if (!is(summaryPolys, "sf"))
    summaryPolys <- sf::st_as_sf(summaryPolys)

  polyNames <- unique(summaryPolys[[polyCol]])

  if (is.null(funList)) {
    funList <- default_landscape_metrics()
  }
  names(funList) <- funList

  oldPlan <- future::plan() |>
    tweak(workers = pemisc::optimalClusterNum(5000, length(vtm))) |>
    future::plan()
  on.exit(future::plan(oldPlan), add = TRUE)

  fragStats <- future.apply::future_lapply(vtm, function(f) {
    r <- terra::rast(f)
    byPoly <- lapply(polyNames, function(polyName) {
      subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]
      rc <- terra::crop(r, subpoly)
      rcm <- terra::mask(rc, subpoly)
      rcm

      out <- lapply(funList, function(fun) {
        fn <- get(fun)

        fn(rcm)
      })
      names(out) <- funList
      out
    })
    names(byPoly) <- paste(tools::file_path_sans_ext(basename(f)), polyNames , sep = "_") ## vegTypeMap_yearXXXX_polyName

    byPoly
  }, future.packages = c("landscapemetrics", "nrvtools", "sf", "terra"), future.seed = TRUE)
  names(fragStats) <- basename(dirname(vtm)) ## repXX

  fragStats <- purrr::transpose(lapply(fragStats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(fragStats)))

  frag_stat_df <- lapply(fragStats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)

    labels <- purrr::transpose(strsplit(names(x), "[.]"))
    labels1 <- unlist(labels[[1]])
    labels2 <- gsub("vegTypeMap", "", unlist(labels[[2]]))
    labels2a <- purrr::transpose(strsplit(labels2, "_"))
    labels2a2 <- unlist(labels2a[[2]]) ## year
    labels2a3 <- if (length(labels2a) == 3) {
      unlist(labels2a[[3]]) ## subpoly
    } else if (length(labels2a) == 4) {
      paste0(unlist(labels2a[[3]]), "_", unlist(labels2a[[4]])) ## subpoly w/ intersection
    } else {
      stop("polyName contains too many underscores")
    }

    vtmReps <- as.integer(gsub("rep", "", labels1))
    vtmTimes <- as.integer(gsub("year", "", labels2a2))
    vtmStudyAreas <- labels2a3

    df <- do.call(rbind, x) |>
      dplyr::mutate(rep = vtmReps, time = vtmTimes, poly = vtmStudyAreas) |>
      dplyr::group_by(time, poly) |>
      dplyr::summarise(
        N = length(value),
        mm = ifelse(N > 0, min(value, na.rm = TRUE), NA_real_),
        mn = ifelse(N > 0, mean(value, na.rm = TRUE), NA_real_),
        mx = ifelse(N > 0, max(value, na.rm = TRUE), NA_real_),
        sd = ifelse(N > 0, sd(value, na.rm = TRUE), NA_real_),
        se = ifelse(N > 0, sd / sqrt(N), NA_real_),
        ci = ifelse(N > 1, se * qt(0.975, N - 1), NA_real_)
      )
  })
  names(frag_stat_df) <- funList

  return(frag_stat_df)
}
