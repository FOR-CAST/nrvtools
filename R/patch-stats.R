utils::globalVariables(c(
  "class", "id", "layer", "level", "metric"
))

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

#' Calculate patch statistics (metrics)
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
#' @seelaso [patchStatsSeral()]
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
#' @seelaso [patchStats()]
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
      dt <- fn(ssm)
      message("...done!")

      dt
    })
    names(out) <- funList
    out
  })
  names(byPoly) <- paste(tools::file_path_sans_ext(basename(ssm)), polyNames, sep = "_") ## seralStageMap_yearXXXX_polyName

  byPoly
}
