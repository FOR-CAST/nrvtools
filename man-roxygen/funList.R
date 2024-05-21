#' @param funList optional list of character strings specifying functions to use for calculating
#' landscape or patch statistics (e.g., `list("lsm_l_area_mn")`).
#' These functions must work analogously to those in \pkg{landscapemetrics}.
#' If not specified by the user (i.e., `funList = NULL`), default function names are used:
#' see [default_landscape_metrics()], [default_patch_metrics()], [default_patch_metrics_seral()].
