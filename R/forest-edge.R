## Codes of the forest / non-forest map made by forest_nonforest_map(): class 1 is the forest, whose
## edge density nrv_forest_edge_density() reports.
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
#' @seealso [nrv_forest_edge_density()]
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

#' Forest edge density per reporting polygon
#'
#' The edge density of the forest class (metres of forest / non-forest edge per hectare of landscape,
#' [landscapemetrics::lsm_c_ed()]) in each forest / non-forest map of `fnf`, within each polygon of
#' `summaryPolys`. It is computed by [nrv_metrics_landscape()], so the result has the long format that
#' returns, and a summary of it ([summarize_nrv()], [nrv_replicate_values()]) goes through the code of the
#' other landscape metrics. The rows have `level = "landscape"` and `metric = "forest_ed"`.
#'
#' A polygon with no forest has an edge density of 0; a polygon with no cells in any map has no rows.
#'
#' @param fnf character, the file paths of forest / non-forest maps (see [forest_nonforest_map()]),
#'   named as `vtm` of [nrv_metrics_landscape()]: `<rep>/<prefix>_year<YYYY>.tif`.
#' @template summaryPolys
#' @template polyCol
#'
#' @return a `data.frame` with columns `layer`, `level`, `class`, `id`, `metric`, `value`, `rep`,
#'   `time`, `poly`, as one element of the list [nrv_metrics_landscape()] returns.
#'
#' @export
#' @seealso [forest_nonforest_map()], [nrv_metrics_landscape()]
nrv_forest_edge_density <- function(fnf, summaryPolys, polyCol) {
  raw <- nrv_metrics_landscape(summaryPolys, polyCol, fnf, funList = list("lsm_c_ed"))[["lsm_c_ed"]]

  key <- c("rep", "time", "poly")
  forest <- raw[raw$class == .forestCode, , drop = FALSE]
  ## a map x polygon with cells but no forest class has an edge density of 0, not no value
  cells <- unique(raw[, key, drop = FALSE])
  none <- cells[!do.call(paste, cells) %in% do.call(paste, forest[, key, drop = FALSE]), , drop = FALSE]
  if (nrow(none)) {
    forest <- rbind(forest, cbind(raw[rep(1L, nrow(none)), c("layer", "level", "class", "id", "metric")],
                                  value = 0, none))
  }

  forest$level <- "landscape"
  forest$class <- NA_integer_
  forest$id <- NA_integer_
  forest$metric <- "forest_ed"
  forest[, names(raw), drop = FALSE]
}
