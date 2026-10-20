## One row of the metric table; `unit` is NA for a count, proportion or other unitless value.
.metric_row <- function(level, metric, name, unit = NA_character_) {
  data.frame(level = level, metric = metric, name = name, unit = unit, stringsAsFactors = FALSE)
}

## Names and units of the metrics the default metric lists and nrvtools' own producers return. The
## units are those of each metric's help page: landscapemetrics for the `lsm_*` metrics, the
## producing function's for nrvtools' own. See nrv_metric_info() for the one exception.
.nrv_metric_table <- function() {
  rbind(
    ## landscapemetrics, patch level
    .metric_row("patch", "area", "Patch area", "ha"),
    .metric_row("patch", "enn", "Euclidean nearest-neighbour distance", "m"),
    ## landscapemetrics, class level
    .metric_row("class", "ai", "Aggregation index", "%"),
    .metric_row("class", "area_cv", "Coefficient of variation of patch area", "%"),
    .metric_row("class", "area_mn", "Mean patch area", "ha"),
    .metric_row("class", "area_sd", "Standard deviation of patch area", "ha"),
    .metric_row("class", "ca", "Total class area", "ha"),
    .metric_row("class", "enn_cv", "Coefficient of variation of nearest-neighbour distance", "%"),
    .metric_row("class", "enn_mn", "Mean nearest-neighbour distance", "m"),
    .metric_row("class", "enn_sd", "Standard deviation of nearest-neighbour distance", "m"),
    ## landscapemetrics, landscape level
    .metric_row("landscape", "ai", "Aggregation index", "%"),
    .metric_row("landscape", "area_mn", "Mean patch area", "ha"),
    .metric_row("landscape", "area_sd", "Standard deviation of patch area", "ha"),
    .metric_row("landscape", "cohesion", "Patch cohesion index", "%"),
    .metric_row("landscape", "condent", "Conditional entropy", "bits"),
    .metric_row("landscape", "core_cv", "Coefficient of variation of core area", "%"),
    .metric_row("landscape", "core_mn", "Mean core area", "ha"),
    .metric_row("landscape", "core_sd", "Standard deviation of core area", "ha"),
    .metric_row("landscape", "ed", "Edge density", "m/ha"),
    .metric_row("landscape", "iji", "Interspersion and juxtaposition index", "%"),
    ## nrvtools: patchAges(), patchAreaStatsSeral(), patchSizeClassesSeral(),
    ## interiorForestSeral(), leadingVegByAgeClass()
    .metric_row("patch", "sam_mdn", "Median stand age of a patch", "years"),
    .metric_row("class", "area_min", "Smallest patch area", "ha"),
    .metric_row("class", "area_median", "Median patch area", "ha"),
    .metric_row("class", "area_max", "Largest patch area", "ha"),
    .metric_row("class", "n_patches_below_floor", "Patches below the minimum size"),
    .metric_row("class", "area_ha_below_floor", "Area of patches below the minimum size", "ha"),
    .metric_row("class", "interior_area_ha", "Interior forest area", "ha"),
    .metric_row("class", "interior_prop", "Interior forest, proportion of class area"),
    .metric_row("landscape", "leadingProp", "Proportion of forest area")
  )
}

#' Plain names and units of NRV metrics
#'
#' Looks up the metric codes of the long metric schema (`metric`, at its
#' `level`) and returns a plain-language name and unit for each, for axis
#' titles, captions and tables.
#'
#' The table covers the \pkg{landscapemetrics} metrics of
#' [default_landscape_metrics()], [default_patch_metrics()] and
#' [default_patch_metrics_seral()], and the metrics that nrvtools computes
#' itself: `sam_mdn`, the median stand age of a patch, in years
#' ([patchAges()]); the seral patch area statistics ([patchAreaStatsSeral()],
#' [patchSizeClassesSeral()]); interior forest ([interiorForestSeral()]); and
#' `leadingProp` ([leadingVegByAgeClass()]). The per-size-class metrics, whose
#' codes carry their size class (`n_patches_81_250`, `Npatch_ge100ha`), are not
#' in it.
#'
#' Units are those on each metric's help page, with one exception: the
#' landscapemetrics help pages of the coefficients of variation (`area_cv`,
#' `core_cv`, `enn_cv`) give the unit of the measure they summarise (hectares or
#' metres), but the value they return is `sd / mean * 100`, a percentage.
#' `condent` is in bits, the unit of the default `base = "log2"` of
#' [landscapemetrics::lsm_l_condent()], which nrvtools uses.
#'
#' @param metric Character vector of metric codes. `NULL` (default) returns the
#'   whole table.
#' @param level Optional level of each `metric` (`"patch"`, `"class"` or
#'   `"landscape"`), recycled to its length. `NULL`, or an `NA` element, takes
#'   the first entry for the code, whatever its level.
#'
#' @return A `data.frame` with columns `level`, `metric`, `name`, `unit` (`NA`
#'   for a count, proportion or other unitless value) and `label` (the name, then
#'   the unit in parentheses), one row per element of `metric`. A code not in the
#'   table, or not at the given level, gets `NA` name and unit, and the code as
#'   its `label`.
#'
#' @export
#' @seealso [default_landscape_metrics()], [summarize_nrv()]
#' @examples
#' nrv_metric_info(c("ca", "ed", "sam_mdn"), c("class", "landscape", "patch"))
#' nrv_metric_info("area_mn")$label
nrv_metric_info <- function(metric = NULL, level = NULL) {
  tab <- .nrv_metric_table()
  tab$label <- ifelse(is.na(tab$unit), tab$name, paste0(tab$name, " (", tab$unit, ")"))
  if (is.null(metric)) {
    return(tab)
  }
  metric <- as.character(metric)
  level <- if (is.null(level) || !length(metric)) {
    rep(NA_character_, length(metric))
  } else {
    rep_len(as.character(level), length(metric))
  }
  i <- ifelse(
    is.na(level),
    match(metric, tab$metric),
    match(paste(level, metric), paste(tab$level, tab$metric))
  )
  out <- data.frame(
    level = ifelse(is.na(level), tab$level[i], level),
    metric = metric,
    name = tab$name[i],
    unit = tab$unit[i],
    label = ifelse(is.na(i), metric, tab$label[i]),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}
