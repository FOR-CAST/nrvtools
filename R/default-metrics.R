#' Default metrics
#'
#' @section Default Landscape Metrics:
#'
#' The following landscape-level metrics are calculated using \pkg{landscapemetrics}:
#'
#' - `lsm_l_area_mn`: mean patch area;
#' - `lsm_l_area_sd`: standard deviation of patch area;
#' - `lsm_l_cohesion`, cohesion index;
#' - `lsm_l_condent`: conditional entropy;
#' - `lsm_l_core_cv`: coeff var of core area;
#' - `lsm_l_core_mn`: mean core area;
#' - `lsm_l_core_sd`: std dev of core area;
#' - `lsm_l_ed`: edge density;
#' - `lsm_l_iji`: interspersion and juxtaposition index;
#'
#' @export
#' @rdname default_metrics
default_landscape_metrics <- function() {
  list(
    "lsm_l_area_mn", ## mean patch area
    "lsm_l_area_sd", ## standard deviation of patch area
    "lsm_l_ai", ## aggregation index
    "lsm_l_cohesion", ## cohesion index
    "lsm_l_condent", ## conditional entropy
    "lsm_l_core_cv", ## coeff var of core area
    "lsm_l_core_mn", ## mean core area
    "lsm_l_core_sd", ## std dev of core area
    "lsm_l_ed", ## edge density
    "lsm_l_iji" ## interspersion and juxtaposition index
  )
}

#' @section Default Patch Metrics:
#'
#' The following patch- and class-level metrics are calculated using
#' \pkg{landscapemetrics} and \pkg{nrvtools}:
#'
#' - `patchAges`: patch ages;
#' - `patchAreas`: patch area (uses `lsm_p_area`);
#' - `lsm_c_ai`: aggregation index by class;
#' - `lsm_c_area_cv`: coeff var patch area by class;
#' - `lsm_c_area_mn`: mean patch area by class;
#' - `lsm_c_area_sd`: std dev patch area by class;
#' - `lsm_c_ca`: total class area;
#'
#' @export
#' @rdname default_metrics
default_patch_metrics <- function() {
  list(
    "patchAges",
    "patchAreas",
    "lsm_c_ai",
    "lsm_c_area_cv",
    "lsm_c_area_mn",
    "lsm_c_area_sd",
    "lsm_c_ca"
  )
}

#' @section Default Seral Stage Patch Metrics:
#'
#' The following patch- and class-level metrics are calculated using
#' \pkg{landscapemetrics} and \pkg{nrvtools}:
#'
#' - `patchAreasSeral`: patch area (uses `lsm_p_area`);
#' - `patchAreaStatsSeral`: min / median / max patch area by class;
#' - `patchSizeClassesSeral`: patch counts and area by CEF patch size class;
#' - `lsm_p_enn`, `lsm_c_enn_mn`, `lsm_c_enn_cv`, `lsm_c_enn_sd`: Euclidean
#'   nearest-neighbour (interpatch) distance;
#' - the subset of `default_patch_metrics()` from \pkg{landscapemetrics}
#'   (i.e., prefixed with 'lsm_')
#'
#' Interior forest area is *not* in this list: [interiorForestSeral()] needs the
#' reporting polygons, so it is called alongside `funList` rather than through it.
#'
#' @note patch ages don't make sense here since seral stage (based on age) determines patches
#'
#' @note `lsm_*_enn_*` report the distance between the nearest *cell centres* of
#'   two patches, which is one cell width larger than the gap between them.
#'   Subtract the cell size before comparing against an edge-to-edge distance
#'   measured on polygons.
#'
#' @export
#' @rdname default_metrics
default_patch_metrics_seral <- function() {
  pm_default <- default_patch_metrics()
  lsm_default <- pm_default[grepl("lsm_", pm_default)]
  c(
    list("patchAreasSeral", "patchAreaStatsSeral", "patchSizeClassesSeral"),
    lsm_default,
    list("lsm_p_enn", "lsm_c_enn_mn", "lsm_c_enn_cv", "lsm_c_enn_sd")
  )
}

#' @section Default LandWeb Summary Metrics:
#'
#' The ported v2 `LandWeb_summary` analyses (see [calculateLandWebMetrics()]):
#'
#' - `leadingVegByAgeClass`: proportion of each vegetation class by age class;
#' - `largePatchCounts`: number/total-area of large patches by size class,
#'   vegetation class and age class.
#'
#' @export
#' @rdname default_metrics
default_landweb_metrics <- function() {
  list("leadingVegByAgeClass", "largePatchCounts")
}
