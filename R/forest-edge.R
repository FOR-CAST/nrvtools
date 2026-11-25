## Codes of the forest / non-forest map made by forest_nonforest_map(): class 1 is the forest.
.forestCode <- 1L
.nonForestCode <- 0L

#' Forest / non-forest map from a stand-age map
#'
#' Classifies each pixel of a stand-age map as forest or non-forest. A pixel is **non-forest** if its
#' stand age is below `ageThreshold`, or it has no age (no cohorts) inside the study area, or it is
#' non-forest in `nonForest`. Every other pixel is **forest**. Pixels outside the study area are `NA`.
#'
#' nrvtools does not compute stand age. Pass the map the simulation's own stand-age output uses, such as
#' `LandR::standAgeMapGenerator()` (biomass-weighted age of the cohorts of each pixel).
#'
#' @param age A stand-age `SpatRaster` (years), or the path of one. `NA` is a pixel without cohorts, or
#'   outside the study area.
#' @param ageThreshold Stands younger than this (years) are non-forest. Default 20.
#' @param nonForest Optional `SpatRaster` (or path) on the grid of `age`: cells that are not `NA` and
#'   not `0` are non-forest, e.g. a non-forest land-cover mask.
#' @param studyArea Optional polygons (`SpatVector`, `sf`) of the study area. Cells of `age` outside it
#'   are `NA`; cells inside it that `age` has as `NA` are non-forest. Without it, every `NA` in `age` is
#'   taken to be outside the study area, so a pixel without cohorts is `NA`, not non-forest.
#'
#' @return A `SpatRaster` on the grid of `age`, named `forest`: `1` forest, `0` non-forest, `NA`
#'   outside the study area. Its category table labels them "Forest" and "Non-forest".
#'
#' @export
#' @details
#' The map is a classified raster like any other: [nrv_metrics_landscape()] with `funList = "lsm_c_ed"`
#' gives its edge density (class `1`, the forest, in m/ha).
#'
#' @seealso [nrv_metrics_landscape()]
forest_nonforest_map <- function(age, ageThreshold = 20, nonForest = NULL, studyArea = NULL) {
  if (is.character(age)) {
    age <- terra::rast(age)
  }
  stopifnot(inherits(age, "SpatRaster"), is.numeric(ageThreshold), length(ageThreshold) == 1L)

  v <- as.integer(terra::values(age, mat = FALSE) >= ageThreshold)

  if (!is.null(nonForest)) {
    if (is.character(nonForest)) {
      nonForest <- terra::rast(nonForest)
    }
    nf <- terra::values(nonForest, mat = FALSE)
    stopifnot(length(nf) == length(v))
    v[!is.na(nf) & nf != 0 & !is.na(v)] <- .nonForestCode
  }

  out <- terra::rast(age, nlyrs = 1L, names = "forest", vals = v)
  if (!is.null(studyArea)) {
    if (!inherits(studyArea, "SpatVector")) {
      studyArea <- terra::vect(studyArea)
    }
    if (!terra::same.crs(studyArea, out)) {
      studyArea <- terra::project(studyArea, out)
    }
    inside <- terra::rasterize(studyArea, out, field = 1, background = NA)
    out <- terra::ifel(is.na(out) & !is.na(inside), .nonForestCode, out)
    out <- terra::mask(out, inside)
    names(out) <- "forest"
  }
  levels(out) <- data.frame(
    id = c(.nonForestCode, .forestCode),
    values = c("Non-forest", "Forest")
  )
  out
}
