utils::globalVariables(c(
  "class", "id", "layer", "lyr.1", "level", "metric"
))

## The column of a terra categorical RAT (`terra::levels(r)[[1]]`) that holds the raster cell value
## (the integer code matched against `lsm_p_*()$class`). terra names this column "ID" for some rasters
## and "value" for others (e.g. the seral-stage map from `seralStageMapGeneratorBC()`), so match either
## -- anchored so the label column "values" is not picked -- and fall back to the first column (the
## value column by terra convention).
.rat_value_col <- function(rat) {
  hit <- which(grepl("^(id|value)$", names(rat), ignore.case = TRUE))
  if (length(hit) >= 1L) hit[[1L]] else 1L
}

## Resolve a `funList` entry to a function: a bare name ("lsm_c_np") is looked up as before, or an
## explicit "pkg::fun" is resolved from that package's namespace, letting a caller disambiguate or reach
## a function not otherwise visible (GH #1). getExportedValue() loads the namespace as needed.
.get_fun <- function(fun) {
  if (grepl("::", fun, fixed = TRUE)) {
    parts <- strsplit(fun, "::", fixed = TRUE)[[1L]]
    getExportedValue(parts[[1L]], parts[[2L]])
  } else {
    get(fun)
  }
}

#' Relabel integer vegetation-type class codes with their species labels
#'
#' Replaces integer vegetation-type category codes in a `class` column with the
#' matching species labels from a categorical vegetation-type map's RAT
#' (`terra::levels(vtm)[[1]]`). Class-level \pkg{landscapemetrics} metrics
#' (`lsm_c_*()`) report the raw integer category, whereas the nrvtools producers
#' ([patchAges()] / [patchAreas()]) already return labelled classes -- this
#' relabels the former and leaves the latter unchanged. Idempotent: any class
#' value that is not an integer code (e.g. an already-mapped species name) or
#' that has no RAT match is kept as-is. Handles a `class` column stored as
#' numeric (raw metric output) or as character (e.g. after a mixed-type column is
#' written to and read back from parquet).
#'
#' @param df A `data.frame`/`tibble` with a class column (`class_col`); returned
#'   unchanged if `NULL`, empty, or lacking that column.
#' @param vtm A categorical vegetation-type `SpatRaster`; its RAT supplies the
#'   code -> label lookup. A raster with no usable RAT leaves `df` unchanged.
#' @param class_col Name of the class column to relabel (default `"class"`).
#'
#' @return `df` with `class_col` relabelled where codes matched the RAT.
#'
#' @export
#' @seealso [patchStats()], [patchAreas()]
label_vegtype_classes <- function(df, vtm, class_col = "class") {
  if (is.null(df) || !nrow(df) || !(class_col %in% names(df))) {
    return(df)
  }
  rat <- terra::levels(vtm)[[1]]
  if (is.null(rat) || NCOL(rat) < 2L) {
    return(df)
  }
  idcol <- .rat_value_col(rat)
  lblcol <- setdiff(seq_len(NCOL(rat)), idcol)[[1L]]
  code <- suppressWarnings(as.integer(as.character(df[[class_col]])))
  lab <- rat[match(code, rat[[idcol]]), ][[lblcol]]
  df[[class_col]] <- ifelse(is.na(lab), as.character(df[[class_col]]), as.character(lab))
  df
}

#' Forested area (ha) per subregion and leading species
#'
#' Tabulates the forested area, in hectares, of each leading-vegetation species
#' within each subregion of a reporting layer, from a categorical
#' vegetation-type map (the leading-species map). Mirrors the v2 LandWeb /
#' NW_AB comparative-boxplot area calculation: extract the map over the
#' subregion polygons, count cells per species, and multiply by the cell area.
#' An `all_label` ("All species") row per subregion carries the subregion's
#' total forested area.
#'
#' @param vtm A categorical vegetation-type `SpatRaster` (leading-species map);
#'   assumed to have a projected CRS in metres (cell area = `prod(res(vtm))`).
#' @param polys Subregion polygons (`SpatVector`, `sf`, or `Spatial`); reprojected
#'   to `vtm` as needed.
#' @param poly_col Name of the column in `polys` holding the subregion label.
#' @param all_label Label for the per-subregion total row (default
#'   `"All species"`); `NULL` omits the totals.
#'
#' @return A `data.frame` with columns `poly`, `vegCover`, `n_pixels`, `area_ha`.
#'
#' @export
#' @seealso [plot_leading_boxplot()]
subregion_forested_area <- function(vtm, polys, poly_col, all_label = "All species") {
  if (!inherits(polys, "SpatVector")) {
    polys <- terra::vect(polys)
  }
  polys <- terra::project(polys, vtm)
  cell_ha <- prod(terra::res(vtm)) / 1e4 ## projected CRS in metres -> ha
  ext <- terra::extract(vtm, polys) ## ID + the (factor) species-label column
  vcol <- setdiff(names(ext), "ID")[[1L]]
  poly_name <- as.character(polys[[poly_col]][[1L]])
  df <- data.frame(
    poly = poly_name[ext[["ID"]]],
    vegCover = as.character(ext[[vcol]]),
    stringsAsFactors = FALSE
  )
  df <- df[!is.na(df[["vegCover"]]) & !is.na(df[["poly"]]), , drop = FALSE]
  empty <- data.frame(
    poly = character(0),
    vegCover = character(0),
    n_pixels = integer(0),
    area_ha = numeric(0)
  )
  if (!nrow(df)) {
    return(empty)
  }
  per <- df |>
    dplyr::count(.data[["poly"]], .data[["vegCover"]], name = "n_pixels") |>
    dplyr::mutate(area_ha = .data[["n_pixels"]] * cell_ha)
  if (!is.null(all_label)) {
    totals <- per |>
      dplyr::group_by(.data[["poly"]]) |>
      dplyr::summarise(
        n_pixels = sum(.data[["n_pixels"]]),
        area_ha = sum(.data[["area_ha"]]),
        .groups = "drop"
      ) |>
      dplyr::mutate(vegCover = all_label)
    per <- dplyr::bind_rows(per, totals)
  }
  as.data.frame(per[, c("poly", "vegCover", "n_pixels", "area_ha")])
}

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
  spp <- terra::levels(vtm)[[1]]
  ## a tiny / empty subregion (no forested pixels) yields a non-categorical crop (RAT NULL / < 2
  ## cols): nothing to label, return the (already empty) areas rather than erroring.
  if (is.null(spp) || NCOL(spp) < 2L || nrow(areas) == 0L) {
    return(areas[0, , drop = FALSE])
  }
  idcol <- .rat_value_col(spp)
  ## label column = the positional non-value column (terra names it "values"/"category"/etc. -- do
  ## not hard-code "values", or RATs that use another name return NA and leave raw integer classes).
  lblcol <- setdiff(seq_len(NCOL(spp)), idcol)[[1L]]
  sppNames <- spp[match(areas$class, spp[[idcol]]), ][[lblcol]]

  areas <- dplyr::mutate(areas, class = as.character(sppNames))

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
  spp <- terra::levels(vtm)[[1]]
  ## a tiny / empty subregion (no forested pixels) yields a non-categorical crop (RAT NULL / < 2
  ## cols): there are no patches to age, so return an empty result rather than erroring on
  ## `names(spp)[[1L]]` below.
  if (is.null(spp) || NCOL(spp) < 2L || nrow(spp) == 0L) {
    return(data.frame(
      layer = integer(0),
      level = character(0),
      class = character(0),
      id = integer(0),
      metric = character(0),
      value = numeric(0)
    ))
  }
  ptchs <- landscapemetrics::get_patches(vtm)[[1]] ## identify patches for each species (class)
  ptchs$class_0 <- NULL ## class 0 has no forested vegetation (e.g., recently disturbed)
  ## terra's category table (RAT): column 1 is the integer category id, column 2 its label.
  ## Use positional columns, not hard-coded names -- LandR writes them lowercase (`id`/`values`)
  ## whereas other producers use `ID`, which silently mismatched here and renamed every patch to
  ## NA -> `ptchs[[NA]]` -> `terra::values(NULL)`.
  idCol <- names(spp)[[1L]]
  lblCol <- names(spp)[[2L]]
  spp$class <- paste0("class_", spp[[idCol]])
  names(ptchs) <- spp[match(names(ptchs), spp[["class"]]), ][[lblCol]]

  df <- rbindlist(lapply(names(ptchs), function(p) {
    ids <- which(!is.na(ptchs[[p]][]))
    data.frame(
      layer = 1L,
      level = "patch",
      class = p,
      id = terra::values(ptchs[[p]], mat = FALSE)[ids],
      metric = "sam_mdn",
      sam = sam[ids][[1L]] ## stand-age values at the patch cells, as a vector (sam[ids] is a 1-col df)
    ) |>
      dplyr::group_by(layer, level, class, id, metric) |>
      dplyr::summarise(value = median(sam, na.rm = TRUE), .groups = "drop")
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
  ## a tiny / empty subregion (no forested/flammable pixels) yields a non-categorical crop (RAT NULL /
  ## < 2 cols): nothing to label, return the (already empty) areas rather than erroring. Mirrors the
  ## patchAreas() guard.
  if (is.null(seral) || NCOL(seral) < 2L || nrow(areas) == 0L) {
    return(areas[0, , drop = FALSE])
  }
  idcol <- .rat_value_col(seral)
  seralNames <- seral[match(areas[["class"]], seral[[idcol]]), ][["values"]]

  areas <- dplyr::mutate(areas, class = seralNames)

  return(areas)
}

#' Calculate patch statistics/metrics
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
#' @seealso [patchStatsSeral()]
patchStats <- function(vtm, sam, flm, polyNames, summaryPolys, polyCol, funList) {
  f <- terra::rast(flm)
  t <- terra::rast(sam)
  v <- terra::rast(vtm)
  byPoly <- lapply(polyNames, function(polyName) {
    message(paste("  vtm:", basename(vtm), "\n", "  sam:", basename(sam)))
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    fc <- terra::crop(f, subpoly)

    tc <- terra::crop(t, subpoly)
    tcm <- terra::mask(tc, subpoly)
    tcm <- terra::mask(tcm, fc, maskvalues = 0) ## also mask non-flammable pixels

    vc <- terra::crop(v, subpoly)
    vcm <- terra::mask(vc, subpoly)

    ## skip empty subregions (no forested pixels after crop/mask): every metric would either error
    ## (landscapemetrics::get_patches / lsm_* on an all-NA raster -> "attempt to select less than one
    ## element") or be trivially empty. Return an empty table per metric so the subregion still
    ## appears (with no rows) in the assembled output.
    if (terra::global(vcm, "notNA")[[1L]] == 0) {
      empty <- data.frame(
        layer = integer(0),
        level = character(0),
        class = character(0),
        id = integer(0),
        metric = character(0),
        value = numeric(0)
      )
      out <- stats::setNames(replicate(length(funList), empty, simplify = FALSE), funList)
      return(out)
    }

    out <- lapply(funList, function(fun) {
      message(paste("    ... running", fun, "for", polyName))

      fn <- .get_fun(fun)

      if (fun %in% c("patchAges")) {
        dt <- fn(vcm, tcm)
      } else {
        dt <- fn(vcm)
      }
      ## relabel raw integer vegType class codes (class-level lsm_c_* output) with species names via
      ## the VTM RAT; patchAges/patchAreas already return labelled classes and pass through unchanged.
      dt <- label_vegtype_classes(dt, vcm)
      message("...done!")

      dt
    })
    names(out) <- funList
    out
  })
  names(byPoly) <- paste(tools::file_path_sans_ext(basename(vtm)), polyNames, sep = "_") ## vegTypeMap_yearXXXX_polyName

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
#' @seealso [patchStats()]
patchStatsSeral <- function(ssm, flm, polyNames, summaryPolys, polyCol, funList) {
  f <- terra::rast(flm)
  s <- terra::rast(ssm)

  byPoly <- lapply(polyNames, function(polyName) {
    message(paste("  ssm:", basename(ssm)))
    subpoly <- summaryPolys[summaryPolys[[polyCol]] == polyName, ]

    fc <- terra::crop(f, subpoly)

    sc <- terra::crop(s, subpoly)
    scm <- terra::mask(sc, subpoly)
    scm <- terra::mask(scm, fc, maskvalues = 0) ## also mask non-flammable pixels

    ## skip empty subregions (no flammable/forested pixels after crop/mask): every metric would either
    ## error (landscapemetrics::get_patches / lsm_* on an all-NA raster -> "attempt to select less than
    ## one element") or be trivially empty. Return an empty table per metric so the subregion still
    ## appears (with no rows) in the assembled output. Mirrors the patchStats() guard.
    if (terra::global(scm, "notNA")[[1L]] == 0) {
      empty <- data.frame(
        layer = integer(0),
        level = character(0),
        class = character(0),
        id = integer(0),
        metric = character(0),
        value = numeric(0)
      )
      out <- stats::setNames(replicate(length(funList), empty, simplify = FALSE), funList)
      return(out)
    }

    out <- lapply(funList, function(fun) {
      message(paste("    ... running", fun, "for", polyName))
      fn <- .get_fun(fun)
      dt <- fn(scm)
      message("...done!")

      dt
    })
    names(out) <- funList
    out
  })
  names(byPoly) <- paste(tools::file_path_sans_ext(basename(ssm)), polyNames, sep = "_") ## seralStageMap_yearXXXX_polyName

  byPoly
}

#' Calculate patch statistics/metrics
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template flm
#'
#' @template vtm
#'
#' @template sam
#'
#' @template funList
#'
#' @return `data.frame` object
#'
#' @export
#' @seealso [calculatePatchMetricsSeral()]
calculatePatchMetrics <- function(summaryPolys, polyCol, flm, vtm, sam, funList = NULL) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  polyNames <- unique(summaryPolys[[polyCol]])

  if (is.null(funList)) {
    funList <- default_patch_metrics()
  }
  names(funList) <- funList

  ptch_stats <- future.apply::future_mapply(
    patchStats,
    vtm = vtm,
    sam = sam,
    MoreArgs = list(
      flm = flm,
      polyCol = polyCol,
      polyNames = polyNames,
      summaryPolys = summaryPolys,
      funList = funList
    ),
    SIMPLIFY = FALSE,
    future.globals = FALSE,
    future.packages = c("dplyr", "landscapemetrics", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )
  names(ptch_stats) <- basename(dirname(vtm)) ## repXX

  ptch_stats <- purrr::transpose(lapply(ptch_stats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(ptch_stats)))

  ptch_stat_df <- lapply(ptch_stats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    labels <- purrr::transpose(strsplit(names(x), "[.]"))
    labels1 <- unlist(labels[[1]])
    labels2 <- gsub("vegTypeMap", "", unlist(labels[[2]]))
    labels2a <- purrr::transpose(strsplit(labels2, "_"))
    labels2a2 <- unlist(labels2a[[2]]) ## year
    labels2a3 <- if (length(labels2a) == 3) {
      unlist(labels2a[[3]]) ## subpoly
    } else if (length(labels2a) == 4) {
      paste0(unlist(labels2a[[3]]), "_", unlist(labels2a[[4]])) ## subpoly w/ intersection
    } else {
      stop("polyName contains too many underscores")
    }

    vtmReps <- as.integer(gsub("rep", "", labels1))
    vtmTimes <- as.integer(gsub("year", "", labels2a2))
    vtmStudyAreas <- labels2a3

    do.call(
      rbind,
      lapply(seq_along(x), function(i) {
        if (nrow(x[[i]]) == 0) {
          x[[i]] <- data.frame(
            layer = integer(0),
            level = character(0),
            class = character(0),
            id = integer(0),
            metric = character(0),
            value = numeric(0)
          )
        }
        dplyr::mutate(x[[i]], rep = vtmReps[i], time = vtmTimes[i], poly = vtmStudyAreas[i])
      })
    )
  })
  names(ptch_stat_df) <- funList

  return(ptch_stat_df)
}

#' Calculate seral stage patch statistics/metrics
#'
#' @template summaryPolys
#'
#' @template polyCol
#'
#' @template flm
#'
#' @template ssm
#'
#' @template funList
#'
#' @return summary `data.frame` object
#'
#' @export
#' @seealso [calculatePatchMetrics()]
calculatePatchMetricsSeral <- function(summaryPolys, polyCol, flm, ssm, funList = NULL) {
  if (!is(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }

  polyNames <- unique(summaryPolys[[polyCol]])

  if (is.null(funList)) {
    funList <- default_patch_metrics_seral()
  }
  names(funList) <- funList

  ptch_stats <- future.apply::future_mapply(
    FUN = patchStatsSeral,
    ssm = ssm,
    MoreArgs = list(
      flm = flm,
      polyCol = polyCol,
      polyNames = polyNames,
      summaryPolys = summaryPolys,
      funList = funList
    ),
    SIMPLIFY = FALSE,
    future.globals = FALSE,
    future.packages = c("dplyr", "landscapemetrics", "nrvtools", "sf", "terra"),
    future.seed = TRUE
  )
  names(ptch_stats) <- basename(dirname(ssm)) ## repXX

  ptch_stats <- purrr::transpose(lapply(ptch_stats, purrr::transpose)) ## puts fun names as outer list elements

  stopifnot(all(funList == names(ptch_stats)))

  ptch_stat_df <- lapply(ptch_stats, function(x) {
    x <- unlist(x, recursive = FALSE, use.names = TRUE)
    labels <- purrr::transpose(strsplit(names(x), "[.]"))
    labels1 <- unlist(labels[[1]])
    labels2 <- gsub("seralStageMap", "", unlist(labels[[2]]))
    labels2a <- purrr::transpose(strsplit(labels2, "_"))
    labels2a2 <- unlist(labels2a[[2]]) ## year
    labels2a3 <- if (length(labels2a) == 3) {
      unlist(labels2a[[3]]) ## subpoly
    } else if (length(labels2a) == 4) {
      paste0(unlist(labels2a[[3]]), "_", unlist(labels2a[[4]])) ## subpoly w/ intersection
    } else {
      stop("polyName contains too many underscores") ## TODO: improve label extraction to be less fragile
    }

    ssmReps <- as.integer(gsub("rep", "", labels1))
    ssmTimes <- as.integer(gsub("year", "", labels2a2))
    ssmStudyAreas <- labels2a3

    do.call(
      rbind,
      lapply(seq_along(x), function(i) {
        if (nrow(x[[i]]) == 0) {
          x[[i]] <- data.frame(
            layer = integer(0),
            level = character(0),
            class = character(0),
            id = integer(0),
            metric = character(0),
            value = numeric(0)
          )
        }
        dplyr::mutate(x[[i]], rep = ssmReps[i], time = ssmTimes[i], poly = ssmStudyAreas[i])
      })
    )
  })
  names(ptch_stat_df) <- funList

  return(ptch_stat_df)
}
