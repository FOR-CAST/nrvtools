## Small synthetic maps where the protocol's answer is computable by hand.

seral_map <- function(res_m = 120, nrow = 20, ncol = 30, fill = "mid") {
  r <- terra::rast(
    nrows = nrow,
    ncols = ncol,
    xmin = 0,
    xmax = ncol * res_m,
    ymin = 0,
    ymax = nrow * res_m,
    crs = "EPSG:3005"
  )
  terra::values(r) <- match(fill, c("early", "mid", "mature", "old"))
  r
}
label_seral <- function(r) {
  levels(r) <- data.frame(value = 1:4, values = c("early", "mid", "mature", "old"))
  r
}

test_that("cef_patch_params() rejects interior bands with no matching buffer", {
  expect_silent(cef_patch_params())
  expect_error(cef_patch_params(interior_bands = list(old = "nonesuch")), "not in `buffers`")
  p <- cef_patch_params()
  ## mature+old must NOT erase the mature band, else it collapses onto old-only
  expect_false("mature" %in% p$interior_bands$mature_old)
  expect_true("mature" %in% p$interior_bands$old)
})

test_that(".seral_base() strips only the NDT4 leading-species suffixes", {
  expect_identical(
    .seral_base(c("early", "mid_Fir", "mature_Pine", "old_Other")),
    c("early", "mid", "mature", "old")
  )
  expect_identical(.seral_base("old_growth"), "old_growth")
})

test_that("an eight-neighbour grid already encodes the >100 m separation rule at 120 m", {
  ## two old patches one cell apart => a 120 m gap => distinct patches (120 > 100)
  for (gap in 0:2) {
    r <- seral_map()
    r[5:8, 5:8] <- 4L
    c0 <- 9 + gap
    r[5:8, c0:(c0 + 3)] <- 4L
    np <- landscapemetrics::lsm_c_np(r)
    np_old <- np$value[np$class == 4]
    expect_identical(np_old, if (gap == 0) 1 else 2, info = paste("gap", gap))
  }
})

test_that("conditionSeralPatchMap() reports thresholds that round away instead of applying them", {
  r <- label_seral(seral_map(res_m = 120))
  expect_message(conditionSeralPatchMap(r), "NOT APPLIED")
  out <- conditionSeralPatchMap(r, quiet = TRUE)
  realised <- attr(out, "cef_realised")

  ## 1 ha < one 1.44 ha cell, and terra::sieve() needs >= 2 cells: absorption cannot apply
  expect_false(realised$applied[realised$rule == "min patch area (absorption)"])
  ## 100 m < one 120 m cell: bridging is a no-op and must be reported as such
  expect_false(realised$applied[realised$rule == "separation (bridging)"])
  expect_identical(terra::values(out, mat = FALSE), terra::values(r, mat = FALSE))
})

test_that("conditionSeralPatchMap() applies both rules at a 30 m cell", {
  r <- label_seral(seral_map(res_m = 30))
  out <- conditionSeralPatchMap(r, quiet = TRUE)
  realised <- attr(out, "cef_realised")

  ## 30 m cell = 0.09 ha, so 1 ha = 12 cells; 100 m gap = 3 cells
  expect_true(all(realised$applied))
  expect_identical(realised$cells[realised$rule == "separation (bridging)"], 3L)
  expect_identical(realised$cells[realised$rule == "min patch area (absorption)"], 12L)
})

test_that("conditionSeralPatchMap() absorbs sub-threshold patches where representable", {
  r <- seral_map(res_m = 30, nrow = 40, ncol = 40)
  r[10:25, 10:25] <- 4L ## a large old patch
  r[18, 18] <- 1L ## a single-cell early speckle inside it (0.09 ha < 1 ha)
  r <- label_seral(r)
  before <- terra::freq(r)
  out <- conditionSeralPatchMap(r, quiet = TRUE)
  after <- terra::freq(out)
  n_early <- function(f) {
    v <- f$count[as.character(f$value) == "early"]
    if (length(v)) v else 0
  }
  expect_lt(n_early(after), n_early(before))
})

test_that("patchAreaStatsSeral() returns order statistics per class", {
  r <- seral_map()
  r[5:8, 5:8] <- 4L ## 16 cells
  r[12:13, 12:13] <- 4L ## 4 cells
  r <- label_seral(r)
  d <- patchAreaStatsSeral(r)

  old <- d[d$class == "old", ]
  expect_setequal(
    old$metric,
    c("area_min", "area_median", "area_max", "n_patches_below_floor", "area_ha_below_floor")
  )
  cell_ha <- prod(terra::res(r)) / 1e4
  expect_equal(old$value[old$metric == "area_max"], 16 * cell_ha)
  expect_equal(old$value[old$metric == "area_min"], 4 * cell_ha)
})

test_that("patchSizeClassesSeral() bins patches into the protocol's size classes", {
  r <- seral_map(nrow = 60, ncol = 60)
  r[5:20, 5:20] <- 4L ## 256 cells * 1.44 ha = 368.6 ha -> ">250"
  r[40:44, 40:44] <- 4L ## 25 cells * 1.44 ha = 36 ha -> "0-40"
  r <- label_seral(r)
  d <- patchSizeClassesSeral(r)

  old <- d[d$class == "old", ]
  expect_true(all(c("n_patches_0_40", "n_patches_250_up") %in% old$metric))
  expect_equal(old$value[old$metric == "n_patches_0_40"], 1)
  expect_equal(old$value[old$metric == "n_patches_250_up"], 1)
  ## every patch lands in exactly one size class
  expect_equal(sum(old$value[grepl("^n_patches_", old$metric)]), 2)
})

test_that("lsm_p_enn overstates the interpatch gap by one cell width", {
  ## documents the correction callers must apply before comparing to st_distance()
  r <- seral_map()
  r[5:8, 5:8] <- 4L
  r[5:8, 10:13] <- 4L ## one cell (column 9, i.e. 120 m) of clear ground between them
  enn <- landscapemetrics::lsm_p_enn(r)
  gap <- unique(enn$value[enn$class == 4]) - terra::res(r)[1]
  expect_equal(gap, 120)
})

test_that("interiorForestSeral() erodes in vector space where the raster cannot", {
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 4L ## 20x20 old block, surrounded by mid
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"

  d <- interiorForestSeral(r, polys, "region", age = NULL, method = "vector")
  old <- d[d$class == "old", ]
  prop <- old$value[old$metric == "interior_prop"]

  ## the 52 m mid buffer is sub-cell, so eroding on the 120 m grid would retain 100%
  expect_lt(prop, 1)
  expect_gt(prop, 0)
  ## a 20x20 cell block (2400 m across) eroded 52 m on each side keeps (2400-104)^2/2400^2
  expect_equal(prop, (2400 - 2 * 52)^2 / 2400^2, tolerance = 1e-3)
})

test_that("interiorForestSeral() warns when no age layer splits the early bands", {
  r <- seral_map(nrow = 30, ncol = 30)
  r[10:19, 10:19] <- 4L
  r[1:5, 1:5] <- 1L
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"
  expect_warning(interiorForestSeral(r, polys, "region"), "no `age` supplied")
})

test_that("mature+old interior forest does not collapse onto old-only", {
  ## the sibling vector implementation erases the 25 m mature band from mature+old too,
  ## which makes the two targets numerically identical; the protocol does not.
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 3L ## mature ring
  r[15:24, 15:24] <- 4L ## old core
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"

  d <- suppressWarnings(interiorForestSeral(r, polys, "region"))
  a <- d$value[d$class == "mature_old" & d$metric == "interior_area_ha"]
  b <- d$value[d$class == "old" & d$metric == "interior_area_ha"]
  expect_gt(a, b)
})

test_that("the subgrid backend approximates the vector backend", {
  ## a 20x20 cell old block inside mid: interior is analytic, (2400 - 2*52)^2 / 2400^2
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 4L
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"
  expected <- (2400 - 2 * 52)^2 / 2400^2

  vec <- interiorForestSeral(r, polys, "region", method = "vector")
  sg <- interiorForestSeral(r, polys, "region", method = "subgrid", subgrid_factor = 4L)
  prop <- function(d) d$value[d$class == "old" & d$metric == "interior_prop"]

  expect_equal(prop(vec), expected, tolerance = 1e-3)
  ## one sub-cell is 30 m on a 2400 m block, so a few percent is the expected agreement
  expect_equal(prop(sg), expected, tolerance = 0.05)
  expect_equal(prop(sg), prop(vec), tolerance = 0.05)
})

test_that("subgrid_factor must be fine enough for the narrowest band", {
  ## The 25 m mature band is what sets the default. A 60 m sub-cell (factor 2 on a 120 m grid)
  ## cannot express it -- the nearest sub-cell centre is already further than 25 m away, so nothing
  ## is eroded and old interior forest comes back as the whole old extent. A 30 m sub-cell can.
  ## Accuracy is NOT monotone in subgrid_factor for every band: a 52 m band erodes one 60 m ring at
  ## factor 2 and two 30 m rings at factor 4, i.e. the same 60 m either way.
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 3L ## mature surround
  r[15:24, 15:24] <- 4L ## old core, so the only adjacent band is mature (25 m)
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"
  prop <- function(f) {
    d <- suppressWarnings(interiorForestSeral(
      r,
      polys,
      "region",
      method = "subgrid",
      subgrid_factor = f
    ))
    d$value[d$class == "old" & d$metric == "interior_prop"]
  }
  expect_equal(prop(1L), 1) ## no sub-grid at all -> the band is invisible
  expect_equal(prop(2L), 1) ## 60 m sub-cell -> still invisible
  expect_lt(prop(4L), 1) ## 30 m sub-cell -> the band finally bites
})

test_that("both backends agree on which bands each target erases", {
  ## mature+old must keep its mature stands under either backend (see cef_patch_params)
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 3L
  r[15:24, 15:24] <- 4L
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"
  for (m in c("vector", "subgrid")) {
    d <- suppressWarnings(interiorForestSeral(r, polys, "region", method = m))
    a <- d$value[d$class == "mature_old" & d$metric == "interior_area_ha"]
    b <- d$value[d$class == "old" & d$metric == "interior_area_ha"]
    expect_gt(a, b)
  }
})

test_that("interiorForestSeral() validates method and subgrid_factor", {
  r <- label_seral(seral_map(nrow = 10, ncol = 10))
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"
  expect_error(interiorForestSeral(r, polys, "region", method = "nonesuch"), "arg")
  expect_error(interiorForestSeral(r, polys, "region", subgrid_factor = 0L), "subgrid_factor")
})

test_that("patch statistics apply the residual-patch floor and report what it removed", {
  ## 30 m cells: a single cell is 0.09 ha, so sub-1-ha speckle is representable and must be excluded
  r <- seral_map(res_m = 30, nrow = 60, ncol = 60)
  r[10:29, 10:29] <- 4L ## 400 cells * 0.09 = 36 ha
  r[40, 40] <- 4L ## one cell = 0.09 ha, below the 1 ha floor
  r[45, 45] <- 4L ## another
  r <- label_seral(r)

  d <- patchAreaStatsSeral(r)
  old <- d[d$class == "old", ]
  ## the floor must set the minimum, not the speckle
  expect_equal(old$value[old$metric == "area_min"], 36, tolerance = 1e-6)
  expect_equal(old$value[old$metric == "n_patches_below_floor"], 2)
  expect_equal(old$value[old$metric == "area_ha_below_floor"], 0.18, tolerance = 1e-6)

  sz <- patchSizeClassesSeral(r)
  szo <- sz[sz$class == "old", ]
  ## only the 36 ha patch is counted; the two speckles are reported separately
  expect_equal(sum(szo$value[grepl("^n_patches_[0-9]", szo$metric)]), 1)
  expect_equal(szo$value[szo$metric == "n_patches_below_floor"], 2)
})

test_that("the floor is inert where a single cell already exceeds it", {
  ## at 120 m one cell is 1.44 ha, so nothing can fall below a 1 ha floor
  r <- seral_map(nrow = 30, ncol = 30)
  r[10:19, 10:19] <- 4L
  r[25, 25] <- 4L ## a single 1.44 ha cell -- still above the floor
  r <- label_seral(r)
  d <- patchAreaStatsSeral(r)
  old <- d[d$class == "old", ]
  expect_equal(old$value[old$metric == "n_patches_below_floor"], 0)
  expect_equal(old$value[old$metric == "area_min"], 1.44, tolerance = 1e-6)
})

test_that("unclassified land is never counted as patch area or interior forest", {
  ## mirrors a defect in the sibling vector implementation, where the interior-forest target was
  ## not restricted to the mature/old classes and half of reported old interior forest was
  ## unclassified land.
  r <- seral_map(nrow = 40, ncol = 40)
  r[10:29, 10:29] <- 4L
  r[1:5, 1:40] <- NA ## a band of unclassified land
  r <- label_seral(r)
  polys <- sf::st_as_sf(terra::as.polygons(terra::ext(r), crs = terra::crs(r)))
  polys$region <- "all"

  for (m in c("vector", "subgrid")) {
    d <- interiorForestSeral(r, polys, "region", method = m)
    tot <- d$value[d$class == "old" & d$metric == "interior_area_ha"]
    ## old is a 20x20 block of 1.44 ha cells = 576 ha; interior can never exceed that
    expect_lte(tot, 576 + 1e-6)
    expect_gt(tot, 0)
  }
  ## and the NA band contributes no patches
  expect_false(any(is.na(patchAreaStatsSeral(r)$class)))
})
