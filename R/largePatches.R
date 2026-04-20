utils::globalVariables(c(
  ":=", ".N", ".SD",
  "ac_pos", "combined", "patch", "polygonName", "spp_pos",
  "ageClass", "sizeInHa", "vegCover"
))

#' Calculate large patch areas for one vtm/sam pair
#'
#' For each polygon in `summaryPolys`, classifies pixels by leading species
#' (from `vtm`) and age class (derived from `sam`), then identifies connected
#' patches of each species × age-class combination and computes their area in
#' hectares. A second pass computes patches across all species combined (per
#' age class only). Patches smaller than `minPatchSize` are discarded.
#'
#' @template vtm
#' @template sam
#' @template summaryPolys
#' @template polyCol
#'
#' @param id character or numeric identifier for this replicate, stored in the
#'   `rep` column of the output.
#'
#' @param ageClasses character vector of age class labels, youngest to oldest.
#'   Defaults to `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param ageClassCutOffs integer vector of lower age bounds (years) for each
#'   class in `ageClasses`. Defaults to `c(0L, 40L, 80L, 120L)`.
#'
#' @param minPatchSize numeric, minimum patch size in hectares to retain.
#'   Defaults to `100`.
#'
#' @return `data.table` with columns `vegCover`, `ageClass`, `polygonName`,
#'   `rep`, and `sizeInHa`. Rows with `vegCover == "All_species"` give patch
#'   sizes summed across all species within each age class.
#'
#' @export
#' @seealso [calculateLargePatches()]
largePatches <- function(
  vtm,
  sam,
  summaryPolys,
  polyCol,
  id,
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs,
  minPatchSize = 100
) {
  v <- terra::rast(vtm)
  s <- terra::rast(sam)

  ## classify SAM into age class indices 1..n_ac
  n_ac <- length(ageClasses)
  n_spp <- nrow(terra::levels(v)[[1]])
  rcl <- cbind(
    from = ageClassCutOffs,
    to = c(ageClassCutOffs[-1L], Inf),
    becomes = seq_along(ageClasses)
  )
  sam_class <- terra::classify(s, rcl, right = FALSE, include.lowest = TRUE)

  spp <- terra::levels(v)[[1]]
  idcol <- which(tolower(names(spp)) %in% c("id", "value"))

  polyNames <- unique(summaryPolys[[polyCol]])

  data.table::rbindlist(lapply(polyNames, function(polyName) {
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    vc <- terra::crop(v, subpoly)
    vcm <- terra::mask(vc, subpoly)
    sc <- terra::crop(sam_class, subpoly)
    scm <- terra::mask(sc, subpoly)

    pixelAreaHa <- prod(terra::res(vcm)) / 1e4

    vvals <- terra::values(vcm, mat = FALSE)
    svals <- terra::values(scm, mat = FALSE)
    spp_idx <- match(vvals, spp[[idcol]]) ## 1-based position in levels table
    keep <- !is.na(vvals) & vvals != 0L & !is.na(svals) & !is.na(spp_idx)

    if (!any(keep)) {
      return(NULL)
    }

    ## encode (spp_idx, ac_idx) as a single integer: (ac - 1) * n_spp + spp_idx
    ## gives unique values in [1, n_spp * n_ac]
    combined_vals <- rep(NA_integer_, length(vvals))
    combined_vals[keep] <- (svals[keep] - 1L) * n_spp + spp_idx[keep]

    combined_rast <- vcm
    terra::values(combined_rast) <- combined_vals

    patch_rast <- terra::patches(combined_rast, directions = 8L)

    combined_v <- terra::values(combined_rast, mat = FALSE)
    patch_v <- terra::values(patch_rast, mat = FALSE)
    valid <- !is.na(combined_v) & !is.na(patch_v)

    dt <- data.table::data.table(combined = combined_v[valid], patch = patch_v[valid])
    dt <- dt[, .(sizeInHa = .N * pixelAreaHa), by = .(combined, patch)]
    dt[, spp_pos := ((combined - 1L) %% n_spp) + 1L]
    dt[, ac_pos := ((combined - 1L) %/% n_spp) + 1L]
    dt[, vegCover := spp[["values"]][spp_pos]]
    dt[, ageClass := ageClasses[ac_pos]]
    dt[, polygonName := polyName]
    dt[, rep := id]
    dt[, `:=`(combined = NULL, patch = NULL, spp_pos = NULL, ac_pos = NULL)]

    by_species <- dt[sizeInHa >= minPatchSize]

    ## all species combined: patches per age class, ignoring species identity
    ac_only_vals <- rep(NA_integer_, length(vvals))
    ac_only_vals[keep] <- svals[keep]

    ac_only_rast <- vcm
    terra::values(ac_only_rast) <- ac_only_vals

    patch_all_rast <- terra::patches(ac_only_rast, directions = 8L)

    ac_v <- terra::values(ac_only_rast, mat = FALSE)
    pall_v <- terra::values(patch_all_rast, mat = FALSE)
    valid_a <- !is.na(ac_v) & !is.na(pall_v)

    dt_all <- data.table::data.table(ac_pos = ac_v[valid_a], patch = pall_v[valid_a])
    dt_all <- dt_all[, .(sizeInHa = .N * pixelAreaHa), by = .(ac_pos, patch)]
    dt_all[, ageClass := ageClasses[ac_pos]]
    dt_all[, vegCover := "All_species"]
    dt_all[, polygonName := polyName]
    dt_all[, rep := id]
    dt_all[, `:=`(ac_pos = NULL, patch = NULL)]

    all_species <- dt_all[sizeInHa >= minPatchSize]

    data.table::rbindlist(list(by_species, all_species), use.names = TRUE)
  }))
}

#' Calculate large patch areas across simulation replicates
#'
#' Applies [largePatches()] across all vtm/sam file pairs in parallel using
#' `future_mapply`, then combines the results into a single `data.table`.
#' The replicate identifier is taken from `basename(dirname(vtm))`.
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
#' @param minPatchSize numeric, minimum patch size in hectares to retain.
#'   Defaults to `100`.
#'
#' @return `data.table` with columns `vegCover`, `ageClass`, `polygonName`,
#'   `rep`, and `sizeInHa`, with rows for all replicates combined.
#'
#' @export
#' @seealso [largePatches()]
calculateLargePatches <- function(
  summaryPolys,
  polyCol,
  vtm,
  sam,
  ageClasses = .ageClasses,
  ageClassCutOffs = .ageClassCutOffs,
  minPatchSize = 100
) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  ids <- basename(dirname(vtm))

  results <- future.apply::future_mapply(
    largePatches,
    vtm = vtm,
    sam = sam,
    id = ids,
    MoreArgs = list(
      summaryPolys = summaryPolys,
      polyCol = polyCol,
      ageClasses = ageClasses,
      ageClassCutOffs = ageClassCutOffs,
      minPatchSize = minPatchSize
    ),
    SIMPLIFY = FALSE,
    future.packages = c("data.table", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )

  data.table::rbindlist(results, use.names = TRUE)
}
