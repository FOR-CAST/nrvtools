## build a categorical SpatRaster from a matrix of integer codes + a RAT
mk_cat <- function(codes, rat, res_m = 2000) {
  r <- terra::rast(
    nrows = nrow(codes),
    ncols = ncol(codes),
    xmin = 0,
    xmax = ncol(codes) * res_m,
    ymin = 0,
    ymax = nrow(codes) * res_m,
    crs = "EPSG:3978"
  )
  terra::values(r) <- as.vector(t(codes))
  levels(r) <- rat
  r
}

vtm_rat <- data.frame(id = 0:3, values = c("", "Pinu_sp", "Popu_sp", "Pice_gla"))
aco <- c(0, 40, 80, 120)
acl <- c("Young", "Immature", "Mature", "Old")

## a continuous age raster whose cells all fall in one age class
mk_age <- function(codes, ageVal, res_m = 2000) {
  r <- terra::rast(
    nrows = nrow(codes),
    ncols = ncol(codes),
    xmin = 0,
    xmax = ncol(codes) * res_m,
    ymin = 0,
    ymax = nrow(codes) * res_m,
    crs = "EPSG:3978"
  )
  terra::values(r) <- ageVal
  r
}

test_that("leadingVegByAgeClass returns the raw schema, zero-filled, proportions sum to 1", {
  codes <- matrix(1L, 5, 5) ## all Pinu_sp
  vtm <- mk_cat(codes, vtm_rat)
  age <- mk_age(codes, 10) ## all Young

  out <- leadingVegByAgeClass(vtm, age, ageClassCutOffs = aco, ageClasses = acl)

  expect_named(out, c("layer", "level", "class", "id", "metric", "value", "metric.1"))
  expect_setequal(out$class, acl) ## zero-filled across every age class
  expect_true("All species" %in% out$metric.1)
  expect_setequal(out$metric, c("leadingProp", "leadingShare"))
  ## all pixels Young/Pinu -> proportion 1 for (Young, Pinu_sp), 0 elsewhere for Pinu
  lead <- out[out$metric == "leadingProp", ]
  expect_equal(lead$value[lead$class == "Young" & lead$metric.1 == "Pinu_sp"], 1)
  expect_equal(lead$value[lead$class == "Immature" & lead$metric.1 == "Pinu_sp"], 0)
  ## proportions within each present species sum to 1
  present <- stats::aggregate(value ~ metric.1, out[out$metric == "leadingProp", ], sum)
  expect_equal(present$value[present$metric.1 == "All species"], 1)
})

test_that("leadingVegByAgeClass reports each species x age class as a share of the forest", {
  ## by row: Pinu, Pinu, Pinu, Popu, non-forest
  codes <- matrix(c(rep(1L, 15), rep(2L, 5), rep(0L, 5)), 5, 5, byrow = TRUE)
  vtm <- mk_cat(codes, vtm_rat)
  age <- mk_age(codes, 10)
  terra::values(age) <- rep(c(130, 10, 10, 50, 10), each = 5) ## by row: Old, Young, Young, Immature
  out <- leadingVegByAgeClass(vtm, age, ageClassCutOffs = aco, ageClasses = acl)

  share <- out[out$metric == "leadingShare", ]
  val <- function(d, ac, sp) d$value[d$class == ac & d$metric.1 == sp]
  expect_equal(nrow(share), length(acl) * 3L) ## Pinu_sp, Popu_sp, All species
  expect_equal(val(share, "Old", "Pinu_sp"), 5 / 20) ## 20 forested cells
  expect_equal(val(share, "Young", "Pinu_sp"), 10 / 20)
  expect_equal(val(share, "Immature", "Popu_sp"), 5 / 20)
  expect_equal(val(share, "Mature", "Pinu_sp"), 0)
  expect_equal(sum(share$value[share$metric.1 != "All species"]), 1)
  expect_equal(val(out[out$metric == "leadingProp", ], "Old", "Pinu_sp"), round(5 / 15, 4))
  ## the All-species share of each age class is its proportion of the forest
  lead <- out[out$metric == "leadingProp" & out$metric.1 == "All species", ]
  allShare <- share[share$metric.1 == "All species", ]
  expect_equal(allShare$value[match(lead$class, allShare$class)], lead$value)
})

test_that("largePatchCounts counts contiguous patches and their area", {
  codes <- matrix(1L, 5, 5) ## one contiguous 5x5 Pinu block, 2000 m cells = 400 ha each
  vtm <- mk_cat(codes, vtm_rat)
  age <- mk_age(codes, 10) ## Young

  out <- largePatchCounts(
    vtm,
    age,
    ageClassCutOffs = aco,
    ageClasses = acl,
    sizeClasses = c(100, 500),
    minSize = 100,
    directions = 4L
  )

  expect_named(out, c("layer", "level", "class", "id", "metric", "value", "metric.1"))
  expect_setequal(
    unique(out$metric),
    c("Npatch_ge100ha", "totalArea_ge100ha", "Npatch_ge500ha", "totalArea_ge500ha")
  )
  pick <- function(metric, veg) {
    out$value[out$metric == metric & out$class == "Young" & out$metric.1 == veg]
  }
  ## one 25-cell patch = 25 * 400 = 10000 ha
  expect_equal(pick("Npatch_ge100ha", "Pinu_sp"), 1)
  expect_equal(pick("totalArea_ge100ha", "Pinu_sp"), 10000)
  expect_equal(pick("Npatch_ge100ha", "All species"), 1)
  ## empty age class zero-filled
  expect_equal(
    out$value[out$metric == "Npatch_ge100ha" & out$class == "Old" & out$metric.1 == "Pinu_sp"],
    0
  )
})

test_that("largePatchCounts honours connectivity (rook vs queen)", {
  ## two Pinu cells on a diagonal, everything else non-forest (code 0)
  codes <- matrix(0L, 3, 3)
  codes[1, 1] <- 1L
  codes[2, 2] <- 1L
  vtm <- mk_cat(codes, vtm_rat)
  age <- mk_age(codes, 10)

  rook <- largePatchCounts(
    vtm,
    age,
    ageClassCutOffs = aco,
    ageClasses = acl,
    sizeClasses = 100,
    directions = 4L
  )
  queen <- largePatchCounts(
    vtm,
    age,
    ageClassCutOffs = aco,
    ageClasses = acl,
    sizeClasses = 100,
    directions = 8L
  )

  n <- function(o) {
    o$value[o$metric == "Npatch_ge100ha" & o$class == "Young" & o$metric.1 == "Pinu_sp"]
  }
  expect_equal(n(rook), 2) ## diagonal cells not connected under rook
  expect_equal(n(queen), 1) ## connected under queen
})

test_that(".age_class_raster validates lengths", {
  expect_snapshot(
    error = TRUE,
    nrvtools:::.age_class_raster(
      mk_age(matrix(1L, 2, 2), 10),
      ageClassCutOffs = c(0, 40),
      ageClasses = acl
    )
  )
})

test_that("calculateLandWebMetrics parses rep/time/poly and pools across rep x year via summarize_nrv", {
  skip_if_not_installed("arrow")
  codes <- matrix(1L, 6, 6)
  root <- withr::local_tempdir()
  poly <- sf::st_as_sf(sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(12000, 0), c(12000, 12000), c(0, 12000), c(0, 0)))),
    crs = 3978
  ))
  poly$Name <- "Zone1"

  vtmF <- character()
  ageF <- character()
  for (rp in c("rep01", "rep02")) {
    d <- file.path(root, rp)
    dir.create(d)
    for (yr in c("0601", "0602")) {
      vf <- file.path(d, sprintf("vegTypeMap_year%s.tif", yr))
      af <- file.path(d, sprintf("timeSinceFire_year%s.tif", yr))
      terra::writeRaster(mk_cat(codes, vtm_rat), vf)
      terra::writeRaster(mk_age(codes, 10), af)
      vtmF <- c(vtmF, vf)
      ageF <- c(ageF, af)
    }
  }

  raw <- calculateLandWebMetrics(
    poly,
    "Name",
    vtm = vtmF[1:2],
    age = ageF[1:2], ## rep01, both years
    funList = "leadingVegByAgeClass",
    ageClassCutOffs = aco,
    ageClasses = acl
  )
  lead <- raw[["leadingVegByAgeClass"]]
  expect_true(all(c("rep", "time", "poly") %in% names(lead)))
  expect_setequal(lead$time, c(601L, 602L))
  expect_equal(unique(lead$poly), "Zone1")

  aggRoot <- file.path(root, "_agg")
  byRep <- split(vtmF, basename(dirname(vtmF)))
  byRepA <- split(ageF, basename(dirname(ageF)))
  for (rp in names(byRep)) {
    rw <- calculateLandWebMetrics(
      poly,
      "Name",
      vtm = byRep[[rp]],
      age = byRepA[[rp]],
      funList = "leadingVegByAgeClass",
      ageClassCutOffs = aco,
      ageClasses = acl
    )
    write_nrv_parquet(tidy_nrv_metrics(rw), aggRoot, replicate = rp)
  }
  env <- summarize_nrv(aggRoot, id_cols = c("poly", "level", "class", "metric", "metric.1"))
  expect_false("time" %in% names(env)) ## pooled across summary years
  expect_equal(unique(env$n_reps), 4L) ## 2 reps x 2 years
})

test_that("plot_nrv_distribution returns a ggplot and NULL on empty", {
  df <- data.frame(
    value = c(1, 2, 3, 4, 5, 6),
    class = "Young",
    metric = "Npatch_ge100ha",
    metric.1 = "Pinu_sp",
    poly = "Zone1",
    check.names = FALSE
  )
  cc <- data.frame(
    value = 4,
    class = "Young",
    metric = "Npatch_ge100ha",
    metric.1 = "Pinu_sp",
    poly = "Zone1"
  )
  expect_s3_class(plot_nrv_distribution(df, cc = cc), "ggplot")
  expect_null(plot_nrv_distribution(data.frame()))
})

test_that(".parse_metric_labels() survives a layer mixing 1- and 2-token poly names", {
  ## calculateLandWebMetrics() used to split on "_" and purrr::transpose(), which requires every
  ## element to have the same number of tokens. The tenure layer mixes "ANC" with
  ## "DawsonCreek_TSA", so transpose recombined them into the cartesian product of tokens
  ## ("ANC_Edson", "Canfor_GrandePr", ...) and dropped the 1-token names entirely.
  labels <- c(
    "rep01.vegTypeMap_year0700_ANC",
    "rep01.vegTypeMap_year0700_DawsonCreek_TSA",
    "rep02.vegTypeMap_year0950_WeyCo_GrandePr",
    "rep05.standAgeMap_year1000_Boreal PLain WF_Hinton"
  )
  out <- .parse_metric_labels(labels)

  expect_equal(out$rep, c(1L, 1L, 2L, 5L))
  expect_equal(out$time, c(700L, 700L, 950L, 1000L))
  expect_equal(
    out$poly,
    c("ANC", "DawsonCreek_TSA", "WeyCo_GrandePr", "Boreal PLain WF_Hinton")
  )
  ## no name is fabricated by recombination, and none is lost
  expect_length(unique(out$poly), 4L)
})
