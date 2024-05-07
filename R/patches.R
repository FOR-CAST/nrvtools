utils::globalVariables(c(
  "class", "id", "layer", "level", "metric", "N", "poly", "sd",  "se", "time", "value"
))

#' Default patch metrics
#'
#' @export
default_patch_metrics <- function() {
  list(
    "patchAges", ## nrvtools
    "patchAreas" ## nrvtools
  )
}

#' Calculate areas for each patch (per species)
#'
#' @template vtm
#'
#' @return `tibble` object from `landscapemetrics::lsm_p_area`
#'
#' @export
#' @seealso [patchAreasSeral()]
patchAreas <- function(vtm) {
  areas <- landscapemetrics::lsm_p_area(vtm)
  areas <- areas[areas$class != 0, ] ## class 0 has no forested vegetation (e.g., recently disturbed)
  spp <- raster::levels(vtm)[[1]]
  sppNames <- spp[match(areas$class, spp[["id"]]), ][["values"]]

  areas <- dplyr::mutate(areas, class = sppNames)

  return(areas)
}

#' Calculate median stand age for each patch (per species)
#'
#' @template vtm
#'
#' @template sam
#'
#' @return `tibble` object from `landscapemetrics::lsm_p_area`
#'
#' @export
patchAges <- function(vtm, sam) {
  ptchs <- landscapemetrics::get_patches(vtm)[[1]] ## identify patches for each species (class)
  ptchs$class_0 <- NULL ## class 0 has no forested vegetation (e.g., recently disturbed)
  spp <- raster::levels(vtm)[[1]]
  spp$class <- paste0("class_", spp[["ID"]])
  names(ptchs) <- spp[match(names(ptchs), spp[["class"]]), ][["values"]]

  df <- rbindlist(lapply(names(ptchs), function(p) {
    ids <- which(!is.na(ptchs[[p]][]))
    data.frame(
      layer = 1L,
      level = "patch",
      class = p,
      id = terra::values(ptchs[[p]], mat = FALSE)[ids],
      metric = "sam_mdn",
      sam = sam[ids]
    ) |>
      dplyr::group_by(layer, level, class, id, metric) |>
      dplyr::summarise(value = median(sam, na.rm = TRUE))
  }))

  return(df)
}

#' Calculate median stand age for each patch (by seral stage)
#'
#' @template ssm
#'
#' @return `tibble` object from `landscapemetrics::lsm_p_area`
#'
#' @export
#' @seealso [patchAreas()]
patchAreasSeral <- function(ssm) {
  areas <- landscapemetrics::lsm_p_area(ssm)
  seral <- terra::levels(ssm)[[1]]
  seralNames <- seral[match(areas$class, seral[["id"]]), ][["values"]]

  areas <- dplyr::mutate(areas, class = seralNames)

  return(areas)
}

#' Calculate patch statistics/metrics
#'
#' @template vtm
#'
#' @template sam
#'
#' @template flm
#'
#' @template polyNames
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template funList
#'
#' @return nested list of summary data.frames containing patch statistics by summary polygon
#'
#' @export
#' @seealso [patchStatsSeral()]
patchStats <- function(vtm, sam, flm, polyNames, summaryPolys, polyCol, funList) {
  f <- raster::raster(flm)
  t <- raster::raster(sam)
  v <- raster::raster(vtm)
  byPoly <- lapply(polyNames, function(polyName) {
    message(paste("  vtm:", basename(vtm), "\n",
                  "  sam:", basename(sam)))
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    fc <- raster::crop(f, subpoly)

    tc <- raster::crop(t, subpoly)
    tcm <- raster::mask(tc, subpoly)
    tcm <- raster::mask(tc, fc, maskvalue = 0) ## mask non-flammable pixels

    vc <- raster::crop(v, subpoly)
    vcm <- raster::mask(vc, subpoly)

    out <- lapply(funList, function(fun) {
      message(paste("    ... running", fun, "for", polyName))

      fn <- get(fun)

      if (fun %in% c("patchAges")) {
        dt <- fn(vcm, tcm)
      } else {
        dt <- fn(vcm)
      }
      message("...done!")

      dt
    })
    names(out) <- funList
    out
  })
  names(byPoly) <- paste(tools::file_path_sans_ext(basename(vtm)), polyNames , sep = "_") ## vegTypeMap_yearXXXX_polyName

  byPoly
}

#' Calculate patch statistics (metrics) based on seral stages
#'
#' @template ssm
#'
#' @template flm
#'
#' @template polyNames
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template funList
#'
#' @export
#' @seealso [patchStats()]
patchStatsSeral <- function(ssm, flm, polyNames, summaryPolys, polyCol, funList) {
  f <- raster::raster(flm)
  s <- raster::raster(ssm)
  byPoly <- lapply(polyNames, function(polyName) {
    message(paste("  ssm:", basename(ssm)))
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    fc <- raster::crop(f, subpoly)

    sc <- raster::crop(s, subpoly)
    scm <- raster::mask(sc, subpoly)
    scm <- raster::mask(sc, fc, maskvalue = 0) ## mask non-flammable pixels

    out <- lapply(funList, function(fun) {
      message(paste("    ... running", fun, "for", polyName))
      fn <- get(fun)
      dt <- fn(scm)
      message("...done!")

      dt
    })
    names(out) <- funList
    out
  })
  names(byPoly) <- paste(tools::file_path_sans_ext(basename(ssm)), polyNames, sep = "_") ## seralStageMap_yearXXXX_polyName

  byPoly
}

#' Calculate patch statistics/metrics
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template flm
#'
#' @template vtm
#'
#' @template sam
#'
#' @param funList list of character strings specifying function names used to calculate summary
#'                patch metrics. When `NULL`, [default_patch_metrics()] is used.
#'
#' @return summary `data.frame` object
#'
#' @export
#' @seealso [calculatePatchMetricsSeral()]
calculatePatchMetrics <- function(summaryPolys, polyCol, flm, vtm, sam, funList = NULL) {
  if (!is(summaryPolys, "sf"))
    summaryPolys <- sf::st_as_sf(summaryPolys)

  polyNames <- unique(summaryPolys[[polyCol]])

  funList <- default_patch_metrics()
  names(funList) <- funList

  oldPlan <- future::plan() |>
    tweak(workers = pemisc::optimalClusterNum(5000, length(vtm))) |>
    future::plan()
  on.exit(plan(oldPlan), add = TRUE)

  ptch_stats <- future.apply::future_mapply(
    patchStats, vtm = vtm, sam = sam,
    MoreArgs = list(
      flm = flm,
      polyCol = polyCol,
      polyNames = polyNames,
      summaryPolys = summaryPolys,
      funList = funList
    ),
    SIMPLIFY = FALSE,
    future.globals = funList,
    future.packages = c("dplyr", "landscapemetrics", "nrvtools", "sf", "terra")
  )
  names(ptch_stats) <- basename(dirname(vtm)) ## repXX

  ptch_stats <- purrr::transpose(lapply(ptch_stats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(ptch_stats)))

  ptch_stat_df <- lapply(ptch_stats, function(x) {
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

    df <- do.call(rbind, lapply(seq_along(x), function(i) {
      if (nrow(x[[i]]) == 0) {
        x[[i]] <- data.frame(layer = integer(0), level = character(0), class = character(0),
                             id = integer(0), metric = character(0), value = numeric(0))

      }
      dplyr::mutate(x[[i]], rep = vtmReps[i], time = vtmTimes[i], poly = vtmStudyAreas[i]) |>
        dplyr::group_by(class, time, poly, metric) |>
        dplyr::summarise(
          N = length(value),
          mm = ifelse(N > 0, min(value, na.rm = TRUE), NA_real_),
          mn = ifelse(N > 0, mean(value, na.rm = TRUE), NA_real_),
          mx = ifelse(N > 0, max(value, na.rm = TRUE), NA_real_),
          sd = ifelse(N > 0, sd(value, na.rm = TRUE), NA_real_),
          se = ifelse(N > 0, sd / sqrt(N), NA_real_),
          ci = ifelse(N > 1, se * qt(0.975, N - 1), NA_real_)
        )
    }))

    df
  })
  names(ptch_stat_df) <- funList

  return(ptch_stat_df)
}

#' Calculate patch statistics/metrics
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template flm
#'
#' @template ssm
#'
#' @return summary `data.frame` object
#'
#' @export
#' @seealso [calculatePatchMetrics()]
calculatePatchMetricsSeral <- function(summaryPolys, polyCol, flm, ssm) {
  if (!is(summaryPolys, "sf"))
    summaryPolys <- sf::st_as_sf(summaryPolys)

  polyNames <- unique(summaryPolys[[polyCol]])

  funList <- list("patchAreasSeral") ## patch ages don't make sense here since age determines patches
  names(funList) <- funList

  oldPlan <- future::plan() |>
    tweak(workers = pemisc::optimalClusterNum(5000, length(ssm))) |>
    future::plan()
  on.exit(plan(oldPlan), add = TRUE)

  ptch_stats <- future.apply::future_mapply(
    patchStatsSeral, ssm = ssm,
    MoreArgs = list(
      flm = flm,
      polyCol = polyCol,
      polyNames = polyNames,
      summaryPolys = summaryPolys,
      funList = funList
    ),
    SIMPLIFY = FALSE,
    future.globals = funList,
    future.packages = c("dplyr", "landscapemetrics", "nrvtools", "sf", "terra")
  )
  names(ptch_stats) <- basename(dirname(ssm)) ## repXX

  ptch_stats <- purrr::transpose(lapply(ptch_stats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(ptch_stats)))

  ptch_stat_df <- lapply(ptch_stats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    labels <- purrr::transpose(strsplit(names(x), "[.]"))
    labels1 <- unlist(labels[[1]])
    labels2 <- gsub("seralStageMap", "", unlist(labels[[2]]))
    labels2a <- purrr::transpose(strsplit(labels2, "_"))
    labels2a2 <- unlist(labels2a[[2]]) ## year
    labels2a3 <- if (length(labels2a) == 3) {
      unlist(labels2a[[3]]) ## subpoly
    } else if (length(labels2a) == 4) {
      paste0(unlist(labels2a[[3]]), "_", unlist(labels2a[[4]])) ## subpoly w/ intersection
    } else {
      stop("polyName contains too many underscores")
    }

    ssmReps <- as.integer(gsub("rep", "", labels1))
    ssmTimes <- as.integer(gsub("year", "", labels2a2))
    ssmStudyAreas <- labels2a3

    df <- do.call(rbind, lapply(seq_along(x), function(i) {
      if (nrow(x[[i]]) == 0) {
        x[[i]] <- data.frame(layer = integer(0), level = character(0), class = character(0),
                             id = integer(0), metric = character(0), value = numeric(0))

      }
      dplyr::mutate(x[[i]], rep = ssmReps[i], time = ssmTimes[i], poly = ssmStudyAreas[i]) |>
        dplyr::group_by(class, time, poly, metric) |>
        dplyr::summarise(
          N = length(value),
          mm = ifelse(N > 0, min(value, na.rm = TRUE), NA_real_),
          mn = ifelse(N > 0, mean(value, na.rm = TRUE), NA_real_),
          mx = ifelse(N > 0, max(value, na.rm = TRUE), NA_real_),
          sd = ifelse(N > 0, sd(value, na.rm = TRUE), NA_real_),
          se = ifelse(N > 0, sd / sqrt(N), NA_real_),
          ci = ifelse(N > 1, se * qt(0.975, N - 1), NA_real_)
        )
    }))

    df
  })
  names(ptch_stat_df) <- funList

  return(ptch_stat_df)
}
