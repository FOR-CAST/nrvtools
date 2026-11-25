utils::globalVariables(c("age", "B", "forest"))

## Codes of the forest / non-forest map made by forest_nonforest_map(): class 1 is the forest, whose
## edge density nrv_forest_edge_density() reports.
.forestCode <- 1L
.nonForestCode <- 0L

#' Forest / non-forest map from cohort data
#'
#' Classifies each pixel of a pixel group map as forest or non-forest. A pixel is **non-forest** if its
#' biomass-weighted stand age (`sum(age * B) / sum(B)` over the cohorts of its pixel group) is below
#' `ageThreshold`, or it has no cohorts (a pixel group that is `0` or absent from `cd`), or it is
#' non-forest in `nonForest`. Every other pixel is **forest**. Pixels outside the study area are `NA`:
#' those that are `NA` in `pgm`, or, when `studyArea` is given, those outside it.
#'
#' The age is that of `LandR::standAgeMapGenerator(weight = "biomass")`, not floored to the decade
#' (the two classify alike at a threshold that is a multiple of 10, such as the default 20).
#'
#' @param cd A `cohortData` table (columns `pixelGroup`, `age`, `B`), or the path of a `.parquet` or
#'   `.qs2` file that [read_cohort_data()] reads.
#' @param pgm A `pixelGroupMap` `SpatRaster`, or the path of one.
#' @param ageThreshold Stands younger than this (years) are non-forest. Default 20.
#' @param nonForest Optional `SpatRaster` (or path) on the grid of `pgm`: cells that are not `NA` and
#'   not `0` are non-forest, e.g. a non-forest land-cover mask.
#' @param studyArea Optional polygons (`SpatVector`, `sf`) of the study area. Cells of `pgm` outside it
#'   are `NA`; cells inside it that `pgm` has as `NA` are non-forest. Without it, `NA` in `pgm` marks
#'   the outside.
#'
#' @return A `SpatRaster` on the grid of `pgm`, named `forest`: `1` forest, `0` non-forest, `NA`
#'   outside the study area. Its category table labels them "Forest" and "Non-forest".
#'
#' @export
#' @seealso [nrv_forest_edge_density()]
forest_nonforest_map <- function(cd, pgm, ageThreshold = 20, nonForest = NULL, studyArea = NULL) {
  if (is.character(cd)) {
    cd <- read_cohort_data(cd, cols = c("pixelGroup", "age", "B"))
  }
  if (is.character(pgm)) {
    pgm <- terra::rast(pgm)
  }
  stopifnot(inherits(pgm, "SpatRaster"), is.numeric(ageThreshold), length(ageThreshold) == 1L)

  ## biomass-weighted stand age per pixel group (the formula of LandR::standAgeMapGenerator())
  cd <- data.table::as.data.table(cd)
  byGroup <- cd[,
    list(forest = as.integer(sum(age * B) / sum(B) >= ageThreshold)),
    by = "pixelGroup"
  ]
  ## a group with no biomass has no weighted age: no cohorts to be forest with
  byGroup[is.na(forest), forest := .nonForestCode]

  ## cells that have a pixel group are looked up; the rest (0, or absent from `cd`) are non-forest
  key <- .pixelGroupKeys(pgm, byGroup$pixelGroup)
  v <- byGroup$forest[match(key, byGroup$pixelGroup)]
  v[is.na(v) & !is.na(key)] <- .nonForestCode

  if (!is.null(nonForest)) {
    if (is.character(nonForest)) {
      nonForest <- terra::rast(nonForest)
    }
    nf <- terra::values(nonForest, mat = FALSE)
    stopifnot(length(nf) == length(v))
    v[!is.na(nf) & nf != 0 & !is.na(v)] <- .nonForestCode
  }

  out <- terra::rast(pgm, nlyrs = 1L, names = "forest", vals = v)
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
