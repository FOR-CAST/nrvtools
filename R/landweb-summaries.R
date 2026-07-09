utils::globalVariables(c(
  ":=", ".N", "N", "NPixels", "ageClass", "vegCover", "sizeInHa", "catKey", "proportion"
))

## The label column of a terra categorical RAT (`terra::levels(r)[[1]]`): the column holding the
## human-readable class label (species / seral / age-class name), as opposed to the integer value
## column found by `.rat_value_col()`. Prefer a conventionally-named column, else the first column
## that is not the value column.
.rat_label_col <- function(rat, idcol = .rat_value_col(rat)) {
  hit <- which(grepl(
    "^(values|value_?labels?|category|factor|label|ageclass)$",
    names(rat),
    ignore.case = TRUE
  ))
  hit <- setdiff(hit, idcol)
  if (length(hit) >= 1L) hit[[1L]] else setdiff(seq_along(rat), idcol)[[1L]]
}

## decode a categorical SpatRaster's cell values to their labels (NA where cell is NA / unmatched).
.cat_labels <- function(ras) {
  rat <- terra::levels(ras)[[1]]
  idc <- .rat_value_col(rat)
  lblc <- .rat_label_col(rat, idc)
  vals <- terra::values(ras, mat = FALSE)
  as.character(rat[[lblc]])[match(vals, rat[[idc]])]
}

## Classify an age (time-since-fire or stand-age) raster into an integer age-class raster with a RAT
## mapping code -> ageClass label. Replicates the v2 LandWeb_summary classification exactly:
## `from = ageClassCutOffs - 0.1`, `to = c(ageClassCutOffs[-1], Inf)` (see LandWebUtils::LargePatches /
## LeadingVegTypeByAgeClass). `ageClassCutOffs` are the n lower bounds for the n `ageClasses`.
.age_class_raster <- function(age, ageClassCutOffs, ageClasses) {
  if (length(ageClassCutOffs) != length(ageClasses)) {
    stop("`ageClassCutOffs` must be the same length as `ageClasses`.", call. = FALSE)
  }
  rcl <- cbind(
    from = ageClassCutOffs - 0.1,
    to = c(ageClassCutOffs[-1], Inf),
    seq_along(ageClasses)
  )
  r <- terra::classify(age, rcl)
  levels(r) <- data.frame(ID = seq_along(ageClasses), ageClass = ageClasses)
  r
}

#' Proportion of a reporting area in each vegetation class by age class
#'
#' Ports the v2 `LandWebUtils::LeadingVegTypeByAgeClass()` analysis onto the
#' \pkg{nrvtools} raw-producer contract. For a single (already reporting-polygon
#' cropped) vegetation-type map and age raster, it bins the age raster into age
#' classes, then for each leading vegetation class computes the proportion of that
#' class's pixels falling in each age class (plus an `"All species"` roll-up over
#' all classes), zero-filling every age-class x vegetation-class combination.
#'
#' The vegetation-type map already encodes the leading species per pixel (produced
#' upstream by `LandR::vegTypeMapGenerator()` with `vegLeadingProportion`), so this
#' function does not recompute "leading".
#'
#' @template vtm
#'
#' @template age
#'
#' @param ageClassCutOffs numeric, the lower bounds of each age class (same length
#'   as `ageClasses`), e.g. `c(0, 40, 80, 120)`.
#'
#' @param ageClasses character, the age-class labels, e.g.
#'   `c("Young", "Immature", "Mature", "Old")`.
#'
#' @return a raw long `data.frame` with columns `layer`, `level`, `class` (the age
#'   class), `id`, `metric` (`"leadingProp"`), `metric.1` (the vegetation class,
#'   incl. `"All species"`) and `value` (the proportion).
#'
#' @export
#' @seealso [largePatchCounts()], [calculateLandWebMetrics()]
leadingVegByAgeClass <- function(vtm, age, ageClassCutOffs, ageClasses) {
  if (is.character(vtm)) {
    vtm <- terra::rast(vtm)
  }
  if (is.character(age)) {
    age <- terra::rast(age)
  }

  ageRas <- .age_class_raster(age, ageClassCutOffs, ageClasses)

  bb <- data.table::data.table(ageClass = .cat_labels(ageRas), vegCover = .cat_labels(vtm))
  bb <- stats::na.omit(bb)
  bb <- bb[nzchar(vegCover)] ## drop non-forest (VTM code 0 -> empty label), cf. patchAreas class != 0

  ## per-species: proportion of each species' pixels in each age class
  tab <- bb[, list(NPixels = .N), by = c("ageClass", "vegCover")]
  tab[, proportion := round(NPixels / sum(NPixels), 4), by = "vegCover"]

  ## all species combined
  tab2 <- bb[, list(NPixels = .N), by = c("ageClass")]
  tab2[, proportion := round(NPixels / sum(NPixels), 4)]
  tab2[, vegCover := "All species"]

  tab <- data.table::rbindlist(list(tab, tab2), use.names = TRUE, fill = TRUE)

  ## zero-fill every ageClass x vegCover combination
  vegClasses <- unique(c(as.character(bb$vegCover), "All species"))
  allCombos <- data.table::CJ(ageClass = ageClasses, vegCover = vegClasses, unique = TRUE)
  tab <- tab[allCombos, on = c("ageClass", "vegCover")]
  tab[is.na(proportion), proportion := 0]

  out <- data.frame(
    layer = 1L,
    level = "landscape",
    class = as.character(tab$ageClass),
    id = NA_integer_,
    metric = "leadingProp",
    value = tab$proportion,
    stringsAsFactors = FALSE
  )
  out[["metric.1"]] <- as.character(tab$vegCover)
  out
}

#' Count of large patches by size class, vegetation class and age class
#'
#' Ports the v2 `LandWebUtils::LargePatches()` + `runHistsLargePatches()` analysis
#' onto the \pkg{nrvtools} raw-producer contract. For a single (already
#' reporting-polygon cropped) vegetation-type map and age raster, it bins the age
#' raster into age classes, forms one categorical raster keyed by the
#' (age class x vegetation class) combination, delineates contiguous same-key
#' patches with [landscapemetrics::get_patches()] (`directions` connectivity,
#' default 4 to match the v2 GDAL polygonize), and for each `sizeClasses` threshold
#' counts the number of patches at least that large and their total area, per
#' (age class x vegetation class). An `"All species"` pass keys patches on age
#' class only. Patches smaller than `minSize` ha are dropped, as in v2.
#'
#' @template vtm
#'
#' @template age
#'
#' @param ageClassCutOffs numeric, the lower bounds of each age class (same length
#'   as `ageClasses`), e.g. `c(0, 40, 80, 120)`.
#'
#' @param ageClasses character, the age-class labels, e.g.
#'   `c("Young", "Immature", "Mature", "Old")`.
#'
#' @param sizeClasses numeric, patch-size thresholds in hectares to tabulate,
#'   e.g. `c(100, 500, 1000, 5000)`.
#'
#' @param minSize numeric, the minimum patch size (ha) retained before tabulation
#'   (default `100`).
#'
#' @param directions patch connectivity passed to
#'   [landscapemetrics::get_patches()]: `4` (rook, matches v2) or `8` (queen).
#'
#' @return a raw long `data.frame` with columns `layer`, `level`, `class` (the age
#'   class), `id`, `metric` (`"Npatch_ge<size>ha"` / `"totalArea_ge<size>ha"`),
#'   `metric.1` (the vegetation class, incl. `"All species"`) and `value`.
#'
#' @export
#' @seealso [leadingVegByAgeClass()], [calculateLandWebMetrics()]
largePatchCounts <- function(
  vtm,
  age,
  ageClassCutOffs,
  ageClasses,
  sizeClasses = c(100, 500, 1000, 5000),
  minSize = 100,
  directions = 4L
) {
  if (is.character(vtm)) {
    vtm <- terra::rast(vtm)
  }
  if (is.character(age)) {
    age <- terra::rast(age)
  }

  ageRas <- .age_class_raster(age, ageClassCutOffs, ageClasses)
  cellHa <- prod(terra::res(vtm)) / 1e4

  ageLab <- .cat_labels(ageRas)
  vegLab <- .cat_labels(vtm)
  vegLab[!nzchar(vegLab)] <- NA_character_ ## drop non-forest (VTM code 0 -> empty label)

  ## per-patch sizes (ha) of a categorical raster built from `keys` (character per cell, NA to skip),
  ## returned as a data.table(catKey, sizeInHa) with 4- or 8-connected contiguity.
  ## (column is `catKey` not `key`: `key` is a reserved `data.table()` argument.)
  patchSizes <- function(keys) {
    keep <- !is.na(keys)
    if (!any(keep)) {
      return(data.table::data.table(catKey = character(0), sizeInHa = numeric(0)))
    }
    kf <- factor(keys)
    catRas <- terra::rast(vtm)
    catRas[!keep] <- NA
    catRas[keep] <- as.integer(kf)[keep]
    gp <- landscapemetrics::get_patches(catRas, directions = directions)[[1L]]
    data.table::rbindlist(lapply(names(gp), function(cl) {
      code <- as.integer(sub("^class_", "", cl))
      pv <- terra::values(gp[[cl]], mat = FALSE)
      pv <- pv[!is.na(pv)]
      if (!length(pv)) {
        return(NULL)
      }
      cells <- tabulate(pv)
      data.table::data.table(catKey = levels(kf)[code], sizeInHa = cells[cells > 0L] * cellHa)
    }))
  }

  sep <- paste0("_", 75757575, "_") ## unlikely to occur otherwise (matches v2)

  ## per-species: patches keyed on (ageClass, vegCover)
  keyBoth <- ifelse(is.na(ageLab) | is.na(vegLab), NA_character_, paste(ageLab, vegLab, sep = sep))
  bySpecies <- patchSizes(keyBoth)
  if (nrow(bySpecies)) {
    parts <- data.table::tstrsplit(bySpecies$catKey, sep, fixed = TRUE)
    bySpecies[, `:=`(ageClass = parts[[1L]], vegCover = parts[[2L]])]
  } else {
    bySpecies[, `:=`(ageClass = character(0), vegCover = character(0))]
  }

  ## all species: patches keyed on ageClass only (forested pixels)
  allSp <- patchSizes(ifelse(is.na(vegLab), NA_character_, ageLab))
  if (nrow(allSp)) {
    allSp[, `:=`(ageClass = catKey, vegCover = "All species")]
  } else {
    allSp[, `:=`(ageClass = character(0), vegCover = character(0))]
  }

  patches <- data.table::rbindlist(
    list(
      bySpecies[, c("ageClass", "vegCover", "sizeInHa")],
      allSp[, c("ageClass", "vegCover", "sizeInHa")]
    ),
    use.names = TRUE
  )
  patches <- patches[sizeInHa >= minSize]

  vegClasses <- unique(c(vegLab[!is.na(vegLab)], "All species"))
  grid <- data.table::CJ(ageClass = ageClasses, vegCover = vegClasses, unique = TRUE)

  out <- data.table::rbindlist(lapply(sizeClasses, function(sz) {
    tab <- patches[
      sizeInHa >= sz,
      list(N = .N, totalArea = sum(sizeInHa)),
      by = c("ageClass", "vegCover")
    ]
    tab <- tab[grid, on = c("ageClass", "vegCover")]
    tab[is.na(N), `:=`(N = 0L, totalArea = 0)]
    data.table::rbindlist(list(
      data.frame(
        layer = 1L,
        level = "class",
        class = tab$ageClass,
        id = NA_integer_,
        metric = sprintf("Npatch_ge%dha", as.integer(sz)),
        value = as.numeric(tab$N),
        "metric.1" = tab$vegCover,
        check.names = FALSE,
        stringsAsFactors = FALSE
      ),
      data.frame(
        layer = 1L,
        level = "class",
        class = tab$ageClass,
        id = NA_integer_,
        metric = sprintf("totalArea_ge%dha", as.integer(sz)),
        value = as.numeric(tab$totalArea),
        "metric.1" = tab$vegCover,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
    ))
  }))
  as.data.frame(out)
}

#' Calculate LandWeb summary metrics (Leading + LargePatches), raw per replicate
#'
#' The \pkg{nrvtools} raw-producer wrapper for the ported LandWeb_summary analyses,
#' analogous to [calculatePatchMetrics()] but (a) passing both the vegetation-type
#' map and an age (time-since-fire or stand-age) raster to each producer, and (b)
#' *not* masking non-flammable pixels (matching v2 `LargePatches`/`LeadingVegType`).
#' For each vegetation-type map in `vtm` (aligned element-wise with `age`), within
#' each summary polygon it crops+masks the maps to the polygon and runs each
#' producer in `funList`, returning the raw per-replicate long tables (one row per
#' replicate x time x polygon x class x metric) for [tidy_nrv_metrics()] ->
#' [write_nrv_parquet()] -> [summarize_nrv()].
#'
#' The NRV envelope for these metrics pools across replicates *and* summary years,
#' so summarize with `time` excluded from the grouping id columns.
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template vtm
#'
#' @template age
#'
#' @template funList
#'
#' @param ... further arguments passed to the producers (e.g. `ageClassCutOffs`,
#'   `ageClasses`, `sizeClasses`, `minSize`, `directions`, `sppEquiv`,
#'   `sppEquivCol`); each producer receives only the arguments matching its formals.
#'
#' @return a named `list` (one element per entry in `funList`), each a raw long
#'   `data.frame` with columns `layer`, `level`, `class`, `id`, `metric`,
#'   `metric.1`, `value`, `rep`, `time`, `poly`.
#'
#' @export
#' @seealso [leadingVegByAgeClass()], [largePatchCounts()], [calculatePatchMetrics()]
calculateLandWebMetrics <- function(summaryPolys, polyCol, vtm, age, funList = NULL, ...) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }
  polyNames <- unique(summaryPolys[[polyCol]])

  if (is.null(funList)) {
    funList <- default_landweb_metrics()
  }
  names(funList) <- funList

  dots <- list(...)

  stats <- future.apply::future_mapply(
    function(v, a) {
      rv <- terra::rast(v)
      ra <- terra::rast(a)
      byPoly <- lapply(polyNames, function(polyName) {
        subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]
        vcm <- terra::mask(terra::crop(rv, subpoly), subpoly)
        acm <- terra::mask(terra::crop(ra, subpoly), subpoly)

        out <- lapply(funList, function(fun) {
          fn <- get(fun)
          argsFn <- dots[intersect(names(dots), names(formals(fn)))]
          do.call(fn, c(list(vtm = vcm, age = acm), argsFn))
        })
        names(out) <- funList
        out
      })
      names(byPoly) <- paste(tools::file_path_sans_ext(basename(v)), polyNames, sep = "_")
      byPoly
    },
    v = vtm,
    a = age,
    SIMPLIFY = FALSE,
    future.globals = FALSE,
    future.packages = c("data.table", "landscapemetrics", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )
  names(stats) <- basename(dirname(vtm)) ## repXX

  stats <- purrr::transpose(lapply(stats, purrr::transpose)) ## fun names as outer list elements
  stopifnot(all(funList == names(stats)))

  stat_df <- lapply(stats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    labels <- purrr::transpose(strsplit(names(x), "[.]"))
    labels1 <- unlist(labels[[1]])
    labels2 <- gsub("vegTypeMap|standAgeMap|timeSinceFire", "", unlist(labels[[2]]))
    labels2a <- purrr::transpose(strsplit(labels2, "_"))
    labels2a2 <- unlist(labels2a[[2]]) ## year
    labels2a3 <- if (length(labels2a) == 3) {
      unlist(labels2a[[3]]) ## subpoly
    } else if (length(labels2a) == 4) {
      paste0(unlist(labels2a[[3]]), "_", unlist(labels2a[[4]])) ## subpoly w/ intersection
    } else {
      stop("polyName contains too many underscores")
    }

    reps <- as.integer(gsub("rep", "", labels1))
    times <- as.integer(gsub("year", "", labels2a2))
    polys <- labels2a3

    do.call(
      rbind,
      lapply(seq_along(x), function(i) {
        dplyr::mutate(x[[i]], rep = reps[i], time = times[i], poly = polys[i])
      })
    )
  })
  names(stat_df) <- funList

  stat_df
}
