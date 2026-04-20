utils::globalVariables(c(
  ".", ":=", ".N",
  "ageClass", "group", "NPixels", "proportion", "vegCover", "zone"
))

#' Tabulate leading vegetation cover by age class for one vtm/sam pair
#'
#' For each reporting polygon in `summaryPolys`, crops and masks the supplied
#' vegetation type and stand age rasters, counts pixels per species × age-class
#' combination, and computes the proportion of each species' pixels that fall
#' in each age class. A separate "All_species" row gives the proportion of all
#' forested pixels in each age class irrespective of species. Missing
#' combinations are filled with zeros so every zone × species × age-class
#' triple is always present in the output.
#'
#' @template vtm
#' @template sam
#' @template summaryPolys
#' @template polyCol
#'
#' @param id character or numeric identifier for this replicate, stored in
#'   the `group` column of the output.
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#'   Defaults to `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param ageClassCutOffs integer vector of lower age bounds (years) for each
#'   class in `ageClasses`. Defaults to `c(0L, 40L, 80L, 120L)`.
#'
#' @return `data.table` with columns `group`, `zone`, `vegCover`, `ageClass`,
#'   `NPixels`, and `proportion`. `proportion` is the fraction of that
#'   species' pixels (within the polygon) in each age class; for
#'   `vegCover == "All_species"` it is the fraction of all forested pixels
#'   in each age class.
#'
#' @export
#' @seealso [calculateLeadingVegTypeByAgeClass()]
leadingVegTypeByAgeClass <- function(
  vtm,
  sam,
  summaryPolys,
  polyCol,
  id,
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs
) {
  v <- terra::rast(vtm)
  s <- terra::rast(sam)

  spp   <- terra::levels(v)[[1]]
  idcol <- which(tolower(names(spp)) %in% c("id", "value"))

  ## ensure Inf caps the uppermost age class
  ageBreaks <- c(ageClassCutOffs, Inf)

  polyNames <- unique(summaryPolys[[polyCol]])

  ## template of every (zone, ageClass, vegCover) combination for zero-filling
  allCombos <- data.table::CJ(
    zone     = polyNames,
    ageClass = ageClasses,
    vegCover = c(spp[["values"]], "All_species"),
    sorted   = FALSE
  )

  per_poly <- data.table::rbindlist(lapply(polyNames, function(polyName) {
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    vc  <- terra::crop(v, subpoly); vcm <- terra::mask(vc, subpoly)
    sc  <- terra::crop(s, subpoly); scm <- terra::mask(sc, subpoly)

    vvals <- terra::values(vcm, mat = FALSE)
    svals <- terra::values(scm, mat = FALSE)
    keep  <- !is.na(vvals) & vvals != 0L & !is.na(svals)

    if (!any(keep)) return(NULL)

    sppNames <- spp[match(vvals[keep], spp[[idcol]]), ][["values"]]
    acVals   <- as.character(
      cut(svals[keep], breaks = ageBreaks, labels = ageClasses,
          include.lowest = TRUE, right = FALSE)
    )

    dt <- data.table::data.table(vegCover = sppNames, ageClass = acVals, zone = polyName)

    ## per-species counts and proportions (sum to 1 per zone × species)
    by_spp <- dt[, .(NPixels = .N), by = .(zone, ageClass, vegCover)]
    by_spp[, proportion := round(NPixels / sum(NPixels), 4L), by = .(zone, vegCover)]

    ## all-species counts and proportions (sum to 1 per zone)
    all_spp <- dt[, .(NPixels = .N), by = .(zone, ageClass)]
    all_spp[, proportion := round(NPixels / sum(NPixels), 4L), by = zone]
    all_spp[, vegCover := "All_species"]

    data.table::rbindlist(list(by_spp, all_spp), use.names = TRUE)
  }))

  ## right-join to allCombos so every combination is present, filling absent ones with zero
  tabulated <- merge(
    per_poly,
    allCombos,
    by  = c("zone", "ageClass", "vegCover"),
    all = TRUE
  )
  tabulated[is.na(NPixels),    NPixels    := 0L]
  tabulated[is.na(proportion), proportion := 0]
  tabulated[, group := id]

  tabulated
}

#' Tabulate leading vegetation cover by age class across simulation replicates
#'
#' Applies [leadingVegTypeByAgeClass()] across all vtm/sam file pairs in
#' parallel using `future_mapply`, then combines the results into a single
#' `data.table`. The replicate identifier is taken from
#' `basename(dirname(vtm))`.
#'
#' @template summaryPolys
#' @template polyCol
#' @template vtm
#' @template sam
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#'   Defaults to `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param ageClassCutOffs integer vector of lower age bounds (years) for each
#'   class in `ageClasses`. Defaults to `c(0L, 40L, 80L, 120L)`.
#'
#' @return `data.table` with columns `group`, `zone`, `vegCover`, `ageClass`,
#'   `NPixels`, and `proportion`, with rows for all replicates combined.
#'
#' @export
#' @seealso [leadingVegTypeByAgeClass()]
calculateLeadingVegTypeByAgeClass <- function(
  summaryPolys,
  polyCol,
  vtm,
  sam,
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs
) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  ids <- basename(dirname(vtm))

  results <- future.apply::future_mapply(
    leadingVegTypeByAgeClass,
    vtm = vtm,
    sam = sam,
    id  = ids,
    MoreArgs = list(
      summaryPolys    = summaryPolys,
      polyCol         = polyCol,
      ageClasses      = ageClasses,
      ageClassCutOffs = ageClassCutOffs
    ),
    SIMPLIFY = FALSE,
    future.packages = c("data.table", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )

  data.table::rbindlist(results, use.names = TRUE)
}
