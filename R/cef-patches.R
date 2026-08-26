utils::globalVariables(c("area_ha", "cls", "n_patches", "size_class"))

#' Forest-biodiversity patch parameters (CEF interim protocol)
#'
#' The landscape-patch parameters from the Interim Assessment Protocol for
#' Forest Biodiversity in British Columbia (Cumulative Effects Framework),
#' section 3.2.2, as implemented by the BC Gov `arcpy` scripts distributed with
#' the protocol. Returned as one list so that raster and vector implementations
#' of the protocol read the same numbers from a single place.
#'
#' `buffers` are the *edge-influence* distances: how far into a mature or old
#' patch an adjacent younger stand is deemed to exert edge influence, keyed by
#' the base seral class of the *neighbouring* stand. The nominal protocol
#' distances are 200 / 100 / 50 / 25 m; the values used here are the ones the
#' arcpy scripts actually apply (100 -> 101, 50 -> 52), nudging each buffer just
#' past the nominal distance so a stand exactly at the threshold is captured.
#'
#' `interior_bands` records *which* buffers are erased for each interior-forest
#' target. The mature+old target erases only the early and mid bands: the 25 m
#' band is the edge influence of *mature* stands, which are themselves part of
#' that patch, so erasing it would collapse mature+old interior forest onto old
#' interior forest. The old-only target erases all four.
#'
#' @param buffers Named numeric of edge-influence distances (m), keyed by base
#'   seral class (`early_u20`, `early_o20`, `mid`, `mature`).
#' @param min_patch_ha Residual patches smaller than this (ha) are absorbed into
#'   the surrounding patch.
#' @param separation_m Same-class patches separated by more than this distance
#'   (m) are distinct patches; closer ones form a single patch.
#' @param size_classes Numeric breaks (ha) for patch size classes, passed to
#'   [base::cut()]; the defaults are the protocol's 0-40 / 41-80 / 81-250 / >250.
#' @param interior_bands Named list mapping each interior-forest target to the
#'   `buffers` entries erased for it.
#'
#' @return A named `list` of the above.
#'
#' @export
#' @seealso [conditionSeralPatchMap()], [interiorForestSeral()]
cef_patch_params <- function(
  buffers = c(early_u20 = 200, early_o20 = 101, mid = 52, mature = 25),
  min_patch_ha = 1,
  separation_m = 100,
  size_classes = c(0, 40, 80, 250, Inf),
  interior_bands = list(
    mature_old = c("early_u20", "early_o20", "mid"),
    old = c("early_u20", "early_o20", "mid", "mature")
  )
) {
  stopifnot(
    is.numeric(buffers),
    !is.null(names(buffers)),
    is.numeric(min_patch_ha),
    length(min_patch_ha) == 1L,
    is.numeric(separation_m),
    length(separation_m) == 1L,
    is.numeric(size_classes),
    length(size_classes) >= 2L,
    is.list(interior_bands),
    length(interior_bands) > 0L
  )
  unknown <- setdiff(unlist(interior_bands), names(buffers))
  if (length(unknown)) {
    stop(
      "`interior_bands` refers to buffers not in `buffers`: ",
      paste(unknown, collapse = ", "),
      call. = FALSE
    )
  }
  list(
    buffers = buffers,
    min_patch_ha = min_patch_ha,
    separation_m = separation_m,
    size_classes = size_classes,
    interior_bands = interior_bands
  )
}

#' Base seral class of a (possibly NDT4-suffixed) seral stage label
#'
#' [seralStageMapGeneratorBC()] emits 16 classes: the four seral stages, each
#' optionally suffixed `_Fir` / `_Pine` / `_Other` for the NDT4 leading-species
#' groups. The CEF buffer table keys on the four base stages, so the suffix is
#' stripped for the buffer lookup while the full label is kept for reporting.
#'
#' @param x Character vector of seral stage labels.
#' @return Character vector of base labels (`early`, `mid`, `mature`, `old`).
#' @keywords internal
.seral_base <- function(x) sub("_(Fir|Pine|Other)$", "", as.character(x))

## Resolve a seral map argument (file path or SpatRaster) to a SpatRaster.
.as_ssm <- function(ssm) if (inherits(ssm, "SpatRaster")) ssm else terra::rast(ssm)

## The (value, label) columns of a categorical raster's RAT, or NULL when absent.
.ssm_rat <- function(r) {
  rat <- terra::levels(r)[[1]]
  if (is.null(rat) || NCOL(rat) < 2L) {
    return(NULL)
  }
  idc <- .rat_value_col(rat)
  lblc <- setdiff(seq_len(NCOL(rat)), idc)[[1L]]
  data.frame(value = rat[[idc]], label = as.character(rat[[lblc]]), stringsAsFactors = FALSE)
}

#' Condition a seral stage map to the CEF patch definition
#'
#' Applies the two *map-level* steps of the CEF patch definition -- bridging
#' same-class stands closer together than `separation_m`, and absorbing residual
#' patches smaller than `min_patch_ha` -- so that every downstream patch metric
#' is computed on a map whose connected components *are* protocol patches.
#'
#' Both steps are resolution-dependent, and at coarse cell sizes either may be
#' inexpressible:
#'
#' - **Bridging.** Two same-class patches on a grid are separated by either 0 m
#'   (touching, including diagonally) or at least one cell width. Where the cell
#'   is wider than `separation_m` -- as at the 120 m cell size typical of these
#'   simulations -- eight-neighbour connectivity already *is* the separation
#'   rule, and no bridging is applied. At finer cell sizes a morphological
#'   closing of `floor(separation_m / res)` cells is applied per class.
#' - **Absorption.** [terra::sieve()] requires a threshold of at least two
#'   cells, so `min_patch_ha` is expressible only where two cells are smaller
#'   than it. Where they are not, absorption is skipped rather than silently
#'   applied at the wrong scale.
#'
#' The realised behaviour is recorded on the returned raster as the attribute
#' `"cef_realised"` and, unless `quiet`, reported via [base::message()]. Callers
#' should surface it: a threshold that rounded away is the difference between
#' applying the protocol and appearing to.
#'
#' @template ssm
#' @param params Parameter list from [cef_patch_params()].
#' @param quiet Logical; suppress the realised-threshold report.
#'
#' @return A categorical `SpatRaster` (the conditioned seral map), carrying a
#'   `"cef_realised"` attribute: a `data.frame` of each nominal threshold, its
#'   realised value in cells and metres or hectares, and whether it was applied.
#'
#' @export
#' @seealso [cef_patch_params()], [interiorForestSeral()]
conditionSeralPatchMap <- function(ssm, params = cef_patch_params(), quiet = FALSE) {
  r <- .as_ssm(ssm)
  res_m <- terra::res(r)[1L]
  cell_ha <- prod(terra::res(r)) / 1e4
  rat <- .ssm_rat(r)

  bridge_cells <- as.integer(floor(params$separation_m / res_m))
  sieve_cells <- as.integer(ceiling(params$min_patch_ha / cell_ha))
  sieve_ok <- sieve_cells >= 2L

  realised <- data.frame(
    rule = c("separation (bridging)", "min patch area (absorption)"),
    nominal = c(sprintf("%g m", params$separation_m), sprintf("%g ha", params$min_patch_ha)),
    cells = c(bridge_cells, if (sieve_ok) sieve_cells else NA_integer_),
    realised = c(
      sprintf("%g m", bridge_cells * res_m),
      if (sieve_ok) sprintf("%g ha", sieve_cells * cell_ha) else "not applied"
    ),
    applied = c(bridge_cells >= 1L, sieve_ok),
    stringsAsFactors = FALSE
  )

  out <- r

  ## bridging: morphological closing per class, only where a gap is representable
  if (bridge_cells >= 1L && !is.null(rat)) {
    w <- bridge_cells * res_m
    for (code in rat$value) {
      m <- terra::ifel(r == code, 1L, NA)
      grown <- terra::buffer(m, width = w)
      ## erode back by the same width: buffer the complement and drop it
      shrunk <- grown & !terra::buffer(terra::ifel(grown, NA, 1L), width = w)
      out <- terra::ifel(shrunk & is.na(out), code, out)
    }
  }

  ## absorption: sieve away sub-threshold patches, filling from the largest neighbour
  if (sieve_ok) {
    out <- terra::sieve(out, threshold = sieve_cells, directions = 8)
  }

  if (!is.null(rat)) {
    levels(out) <- data.frame(value = rat$value, values = rat$label)
  }
  names(out) <- names(r)
  attr(out, "cef_realised") <- realised

  if (!quiet) {
    message(
      "conditionSeralPatchMap(): cell = ",
      format(res_m),
      " m (",
      format(round(cell_ha, 3)),
      " ha)\n",
      paste0(
        "  ",
        realised$rule,
        ": ",
        realised$nominal,
        " -> ",
        realised$realised,
        ifelse(realised$applied, "", "  [NOT APPLIED]"),
        collapse = "\n"
      )
    )
  }
  out
}

## An empty metric table in the shape the nrvtools producers return.
.empty_metrics <- function() {
  data.frame(
    layer = integer(0),
    level = character(0),
    class = character(0),
    id = integer(0),
    metric = character(0),
    value = numeric(0),
    stringsAsFactors = FALSE
  )
}

#' Interior forest area by CEF edge-influence buffers
#'
#' The one part of the CEF patch definition a coarse grid cannot express. Edge
#' influence reaches 25-200 m into a patch, which at a 120 m cell is below the
#' cell width for every band but the widest -- measured on the grid, three of the
#' four buffers erode nothing at all, and interior forest comes back
#' indistinguishable from total mature+old area. So this step is done at a finer
#' resolution than the map's own, where a 25 m strip off a patch boundary is a
#' real area.
#'
#' Each younger class is eroded from the mature+old and old extents at its true
#' edge-influence distance per `params$interior_bands`, at a resolution finer
#' than the map's own -- either in polygon space (`method = "vector"`) or on a
#' refined grid (`method = "subgrid"`). Results are returned as an **area** per
#' subregion, never as a map re-gridded at the original cell size, which would
#' discard exactly the sub-cell precision this step exists to recover.
#'
#' @template ssm
#' @template summaryPolys
#' @template polyCol
#' @param params Parameter list from [cef_patch_params()].
#' @param age Optional stand-age `SpatRaster` (or file path) used to split the
#'   `early` class into the `early_u20` / `early_o20` buffer bands at 20 years.
#'   When `NULL`, all early stands are assigned the `early_o20` band and a
#'   warning is issued -- the 200 m band is then never applied, which
#'   understates edge influence.
#' @param method How to erode the edge-influence buffers.
#'
#'   `"vector"` dissolves each class to a polygon, buffers, and erases. Exact,
#'   but GEOS-bound: cost grows with geometry complexity, not cell count, and on
#'   a district-sized landscape a single snapshot does not finish in an hour.
#'
#'   `"subgrid"` (default) refines the map by `subgrid_factor` and thresholds a
#'   distance transform per band. Linear in cells where the polygon route is
#'   superlinear in geometry complexity, and approximate to within about one
#'   sub-cell. Measured against `"vector"` on a real 120 m seral map at
#'   `subgrid_factor = 4`, interior area agreed to +1.0% (mature+old) and +0.3%
#'   (old). The bias is slightly HIGH, because the distance to a
#'   diagonally-placed stand is marginally overestimated. Use `"vector"` where
#'   exactness matters more than run time.
#'
#'   The difference is what makes a district-sized landscape feasible at all. On
#'   a 7.2M cell map (4.7M active), `"subgrid"` at `subgrid_factor = 4` takes
#'   about 3 minutes per snapshot, where `"vector"` did not finish one snapshot
#'   in an hour.
#' @param subgrid_factor Refinement factor for `method = "subgrid"`; the
#'   sub-cell is `res(ssm) / subgrid_factor`, and cost scales with its square.
#'   `4` (a 30 m sub-cell on a 120 m grid) is the tested default and should be
#'   treated as a floor rather than a preference: at `2` the 25 m mature band is
#'   narrower than a sub-cell and vanishes entirely, which on a district-sized
#'   map reported old interior forest as 85% of old extent against 64% at `4`.
#'
#'   Cost is not only time. A sub-grid layer for a district-sized map is of the
#'   order of 10^8 cells, and peak memory ran to roughly 13 GB in testing, so
#'   size the number of concurrent workers accordingly rather than assuming this
#'   is a light task.
#'
#' @return A long `data.frame` (`layer`, `level`, `class`, `id`, `metric`,
#'   `value`, `poly`) giving `interior_area_ha` and `interior_prop` for each
#'   interior-forest target within each subregion.
#'
#' @export
#' @seealso [cef_patch_params()], [conditionSeralPatchMap()]
interiorForestSeral <- function(
  ssm,
  summaryPolys,
  polyCol,
  params = cef_patch_params(),
  age = NULL,
  method = c("subgrid", "vector"),
  subgrid_factor = 4L
) {
  method <- match.arg(method)
  stopifnot(is.numeric(subgrid_factor), length(subgrid_factor) == 1L, subgrid_factor >= 1L)
  r <- .as_ssm(ssm)
  rat <- .ssm_rat(r)
  if (is.null(rat)) {
    return(.empty_metrics())
  }
  if (!inherits(summaryPolys, "sf")) {
    summaryPolys <- sf::st_as_sf(summaryPolys)
  }
  summaryPolys <- sf::st_transform(summaryPolys, sf::st_crs(terra::crs(r)))

  ## base seral class per cell, with `early` optionally split at 20 years
  base <- .seral_base(rat$label)
  band <- terra::classify(
    r,
    as.matrix(data.frame(from = rat$value, to = match(base, c("early", "mid", "mature", "old")))),
    others = NA
  )
  if (!is.null(age)) {
    a <- .as_ssm(age)
    band <- terra::ifel(band == 1L & a < 20, 0L, band)
  } else if (
    "early_u20" %in%
      unlist(params$interior_bands) &&
      isTRUE(unname(terra::global(terra::ifel(band == 1L, 1L, 0L), "sum", na.rm = TRUE)[[1L]]) > 0)
  ) {
    ## only a concern where early stands are actually present to be split
    warning(
      "interiorForestSeral(): no `age` supplied; all early stands treated as >= 20 years, ",
      "so the ",
      params$buffers[["early_u20"]],
      " m band is never applied.",
      call. = FALSE
    )
  }
  codes <- c(early_u20 = 0L, early_o20 = 1L, mid = 2L, mature = 3L, old = 4L)

  targets <- list(mature_old = c("mature", "old"), old = "old")

  areas <- if (identical(method, "vector")) {
    .interior_vector(band, codes, targets, params, summaryPolys, polyCol)
  } else {
    .interior_subgrid(band, codes, targets, params, summaryPolys, polyCol, subgrid_factor)
  }
  if (is.null(areas) || !nrow(areas)) {
    return(.empty_metrics())
  }

  areas <- areas[areas$total_ha > 0, , drop = FALSE]
  if (!nrow(areas)) {
    return(.empty_metrics())
  }
  do.call(
    rbind,
    lapply(seq_len(nrow(areas)), function(i) {
      data.frame(
        layer = 1L,
        level = "class",
        class = areas$class[i],
        id = NA_integer_,
        metric = c("interior_area_ha", "interior_prop"),
        value = c(areas$interior_ha[i], areas$interior_ha[i] / areas$total_ha[i]),
        poly = areas$poly[i],
        stringsAsFactors = FALSE
      )
    })
  )
}

## Exact interior forest via polygon geometry: dissolve each class, buffer the younger ones at their
## true edge-influence distances, erase, and intersect with the subregions. Correct to the metre, but
## GEOS-bound: the buffer and the repeated difference on a landscape-sized multipolygon dominate, and
## at district size a single snapshot does not finish in an hour.
## @return data.frame(class, poly, total_ha, interior_ha)
#' @noRd
.interior_vector <- function(band, codes, targets, params, summaryPolys, polyCol) {
  polys <- terra::as.polygons(band, dissolve = TRUE)
  if (!nrow(polys)) {
    return(NULL)
  }
  sp <- sf::st_make_valid(sf::st_as_sf(polys))
  names(sp)[1L] <- "code"

  geom_of <- function(keys) {
    g <- sp[sp$code %in% unname(codes[keys]), ]
    if (!nrow(g)) {
      return(NULL)
    }
    sf::st_make_valid(sf::st_union(g))
  }
  bufs <- lapply(names(params$buffers), function(k) {
    g <- geom_of(k)
    if (is.null(g)) NULL else sf::st_make_valid(sf::st_buffer(g, params$buffers[[k]]))
  })
  names(bufs) <- names(params$buffers)
  ha <- function(g) if (is.null(g) || !length(g)) 0 else sum(as.numeric(sf::st_area(g))) / 1e4

  out <- lapply(names(targets), function(tg) {
    total <- geom_of(targets[[tg]])
    if (is.null(total)) {
      return(NULL)
    }
    interior <- total
    for (k in params$interior_bands[[tg]]) {
      if (is.null(bufs[[k]])) {
        next
      }
      interior <- sf::st_make_valid(sf::st_difference(interior, bufs[[k]]))
      if (!length(interior)) {
        break
      }
    }
    do.call(
      rbind,
      lapply(seq_len(nrow(summaryPolys)), function(i) {
        sub <- sf::st_geometry(summaryPolys[i, ])
        data.frame(
          class = tg,
          poly = as.character(summaryPolys[[polyCol]][i]),
          total_ha = ha(suppressWarnings(sf::st_intersection(total, sub))),
          interior_ha = if (!length(interior)) {
            0
          } else {
            ha(suppressWarnings(sf::st_intersection(interior, sub)))
          },
          stringsAsFactors = FALSE
        )
      })
    )
  })
  do.call(rbind, out)
}

## Interior forest via a sub-grid distance transform: refine the map by `fact`, and for each buffer
## band keep the sub-cells whose distance to that band exceeds its edge-influence distance. Linear in
## cells where the polygon route is superlinear in geometry complexity.
##
## `terra::distance()` returns the distance to the nearest cell CENTRE of the band, whereas the buffer
## is measured from the band's EDGE, which is about half a sub-cell nearer -- hence the `- s/2`. The
## residual is O(sub-cell size): it is exact for an axis-aligned boundary and slightly overestimates
## the distance where the nearest stand lies diagonally, which biases interior forest marginally HIGH.
## Raise `subgrid_factor` to shrink it, or use `method = "vector"` for the exact answer.
## @return data.frame(class, poly, total_ha, interior_ha)
#' @noRd
.interior_subgrid <- function(band, codes, targets, params, summaryPolys, polyCol, fact) {
  fact <- as.integer(fact)
  sub <- if (fact > 1L) terra::disagg(band, fact = fact) else band
  s <- terra::res(sub)[1L]
  cell_ha <- prod(terra::res(sub)) / 1e4

  ## zones: rasterize the subregions once onto the sub-grid, then count by zone
  zone <- terra::rasterize(terra::vect(summaryPolys), sub, field = seq_len(nrow(summaryPolys)))
  labels <- as.character(summaryPolys[[polyCol]])
  count_by_zone <- function(mask) {
    z <- terra::zonal(terra::ifel(mask, 1L, 0L), zone, fun = "sum", na.rm = TRUE)
    stats::setNames(as.numeric(z[[2L]]), as.character(z[[1L]]))
  }
  ## NOT `sub %in% codes[...]`: terra's `%in%` is not an S4 group generic, so with terra imported
  ## rather than attached it silently falls through to base::`%in%` and returns a plain logical
  ## vector instead of a SpatRaster. `==` dispatches correctly either way.
  mask_of <- function(keys) {
    Reduce(`|`, lapply(unname(codes[keys]), function(cd) sub == cd))
  }
  nonempty <- function(mask) {
    isTRUE(unname(terra::global(terra::ifel(mask, 1L, 0L), "sum", na.rm = TRUE)[[1L]]) > 0)
  }

  ## Accumulate band by band rather than building every band's keep-mask first and combining: at
  ## district size a sub-grid layer is ~10^8 cells, and holding one per band alongside the target
  ## masks is what drives peak memory. This keeps at most one band mask alive at a time.
  totals <- list()
  acc <- list()
  for (tg in names(targets)) {
    m <- mask_of(targets[[tg]])
    if (nonempty(m)) {
      totals[[tg]] <- count_by_zone(m)
      acc[[tg]] <- m
    }
  }
  if (!length(acc)) {
    return(NULL)
  }
  for (k in unique(unlist(params$interior_bands))) {
    users <- names(acc)[vapply(
      names(acc),
      function(tg) k %in% params$interior_bands[[tg]],
      logical(1)
    )]
    if (!length(users)) {
      next
    }
    band_mask <- sub == codes[[k]]
    if (!nonempty(band_mask)) {
      next ## band absent -> nothing to erode
    }
    keep_k <- (terra::distance(terra::ifel(band_mask, 1L, NA)) - s / 2) > params$buffers[[k]]
    rm(band_mask)
    for (tg in users) {
      acc[[tg]] <- acc[[tg]] & keep_k
    }
    rm(keep_k)
  }

  do.call(
    rbind,
    lapply(names(acc), function(tg) {
      tot <- totals[[tg]]
      int <- count_by_zone(acc[[tg]])
      idx <- names(tot)
      data.frame(
        class = tg,
        poly = labels[as.integer(idx)],
        total_ha = unname(tot) * cell_ha,
        interior_ha = unname(int[idx]) * cell_ha,
        stringsAsFactors = FALSE
      )
    })
  )
}

#' Patch counts and area by CEF patch size class
#'
#' Bins each patch's area into the protocol's size classes and reports, per seral
#' class, the number of patches and the total area in each. Patch size class is a
#' headline CEF biodiversity indicator: a landscape can hold a constant total
#' area of old forest while that area migrates from a few large patches into many
#' small ones, which the class-total metrics alone will not show.
#'
#' @template ssm
#' @param params Parameter list from [cef_patch_params()]; `size_classes`
#'   supplies the breaks.
#'
#' @return A long `data.frame` with `metric` of the form `n_patches_<class>` and
#'   `area_ha_<class>` (e.g. `n_patches_81_250`), one row per seral class x size
#'   class.
#'
#' @export
#' @seealso [patchAreaStatsSeral()], [cef_patch_params()]
patchSizeClassesSeral <- function(ssm, params = cef_patch_params()) {
  r <- .as_ssm(ssm)
  areas <- patchAreasSeral(r)
  if (!nrow(areas)) {
    return(.empty_metrics())
  }
  br <- params$size_classes
  tags <- paste0(
    ifelse(is.finite(br[-length(br)]), format(br[-length(br)], trim = TRUE), "0"),
    "_",
    ifelse(is.finite(br[-1L]), format(br[-1L], trim = TRUE), "up")
  )
  sc <- cut(areas$value, breaks = br, labels = tags, include.lowest = TRUE, right = TRUE)
  d <- data.frame(
    class = as.character(areas$class),
    size_class = as.character(sc),
    area_ha = as.numeric(areas$value),
    stringsAsFactors = FALSE
  )
  d <- d[!is.na(d$size_class), , drop = FALSE]
  if (!nrow(d)) {
    return(.empty_metrics())
  }
  agg <- stats::aggregate(area_ha ~ class + size_class, data = d, FUN = function(x) {
    c(n = length(x), a = sum(x))
  })
  res <- data.frame(
    class = agg$class,
    size_class = agg$size_class,
    n_patches = agg$area_ha[, "n"],
    area_ha = agg$area_ha[, "a"],
    stringsAsFactors = FALSE
  )
  rbind(
    data.frame(
      layer = 1L,
      level = "class",
      class = res$class,
      id = NA_integer_,
      metric = paste0("n_patches_", res$size_class),
      value = as.numeric(res$n_patches),
      stringsAsFactors = FALSE
    ),
    data.frame(
      layer = 1L,
      level = "class",
      class = res$class,
      id = NA_integer_,
      metric = paste0("area_ha_", res$size_class),
      value = as.numeric(res$area_ha),
      stringsAsFactors = FALSE
    )
  )
}

#' Minimum, median and maximum patch area by seral class
#'
#' Complements the \pkg{landscapemetrics} class-level moments (`lsm_c_area_mn` /
#' `_sd` / `_cv`) with the order statistics, matching the patch-area summary the
#' sibling vector implementation of this protocol reports.
#'
#' @template ssm
#'
#' @return A long `data.frame` with `metric` in `area_min`, `area_median`,
#'   `area_max` (hectares), one row per seral class.
#'
#' @export
#' @seealso [patchSizeClassesSeral()], [patchAreasSeral()]
patchAreaStatsSeral <- function(ssm) {
  r <- .as_ssm(ssm)
  areas <- patchAreasSeral(r)
  if (!nrow(areas)) {
    return(.empty_metrics())
  }
  sp <- split(as.numeric(areas$value), as.character(areas$class))
  do.call(
    rbind,
    lapply(names(sp), function(k) {
      data.frame(
        layer = 1L,
        level = "class",
        class = k,
        id = NA_integer_,
        metric = c("area_min", "area_median", "area_max"),
        value = c(min(sp[[k]]), stats::median(sp[[k]]), max(sp[[k]])),
        stringsAsFactors = FALSE
      )
    })
  )
}
