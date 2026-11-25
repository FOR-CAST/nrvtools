## 4 x 4 cells of 10 m (1600 m2 = 0.16 ha). Pixel groups: 1 is old (100 y), 2 is young (10 y), 3 mixes
## equal biomass at 10 and 100 y (weighted age 55), 4 mixes 900 at 10 y with 100 at 100 y (weighted 19),
## 0 has no cohorts and 9 is not in the table.
.fnf_cd <- function() {
  data.table::data.table(
    pixelGroup = c(1L, 2L, 3L, 3L, 4L, 4L),
    age = c(100, 10, 10, 100, 10, 100),
    B = c(500, 500, 100, 100, 900, 100)
  )
}
.fnf_pgm <- function(vals) {
  terra::rast(matrix(vals, 4, 4, byrow = TRUE), crs = "EPSG:32611", extent = terra::ext(0, 40, 0, 40))
}
## left half group 1 (forest), right half group 2 (non-forest): one 40 m edge
.fnf_halves <- function() .fnf_pgm(rep(c(1, 1, 2, 2), 4))

test_that("forest_nonforest_map classifies by biomass-weighted age", {
  pgm <- .fnf_pgm(c(1, 2, 3, 4,  0, 9, NA, 1,  1, 1, 1, 1,  2, 2, 2, 2))
  m <- forest_nonforest_map(.fnf_cd(), pgm)
  expect_s4_class(m, "SpatRaster")
  expect_equal(
    as.vector(terra::values(m)),
    c(1, 0, 1, 0,  0, 0, NA, 1,  1, 1, 1, 1,  0, 0, 0, 0)
  )
  expect_equal(terra::levels(m)[[1]]$values, c("Non-forest", "Forest"))
  ## the threshold moves group 4 (19 y) and group 3 (55 y)
  expect_equal(as.vector(terra::values(forest_nonforest_map(.fnf_cd(), pgm, ageThreshold = 19)))[1:4], c(1, 0, 1, 1))
  expect_equal(as.vector(terra::values(forest_nonforest_map(.fnf_cd(), pgm, ageThreshold = 60)))[1:4], c(1, 0, 0, 0))
})

test_that("forest_nonforest_map honours the non-forest mask and the study area", {
  pgm <- .fnf_halves()
  nf <- terra::rast(pgm, vals = rep(c(0, 1, 0, 0), 4)) ## column 2 is non-forest land cover
  m <- forest_nonforest_map(.fnf_cd(), pgm, nonForest = nf)
  expect_equal(as.vector(terra::values(m))[1:4], c(1, 0, 0, 0))
  ## a study area of the left three columns: outside is NA; a pgm NA inside it is non-forest
  pgm2 <- .fnf_pgm(c(1, NA, 2, 2, rep(c(1, 1, 2, 2), 3)))
  area <- terra::as.polygons(terra::ext(0, 30, 0, 40), crs = "EPSG:32611")
  m2 <- forest_nonforest_map(.fnf_cd(), pgm2, studyArea = area)
  expect_equal(as.vector(terra::values(m2))[1:4], c(1, 0, 0, NA))
  ## in another CRS, the same
  m3 <- forest_nonforest_map(.fnf_cd(), pgm2, studyArea = terra::project(area, "EPSG:4326"))
  expect_equal(as.vector(terra::values(m3))[1:4], c(1, 0, 0, NA))
})

test_that("forest_nonforest_map reads cohortData and the map from files", {
  skip_if_not_installed("qs2")
  d <- withr::local_tempdir()
  qs2::qs_save(.fnf_cd(), file.path(d, "cd.qs2"))
  terra::writeRaster(.fnf_halves(), file.path(d, "pgm.tif"))
  expect_equal(
    as.vector(terra::values(forest_nonforest_map(file.path(d, "cd.qs2"), file.path(d, "pgm.tif")))),
    as.vector(terra::values(forest_nonforest_map(.fnf_cd(), .fnf_halves())))
  )
})

test_that("nrv_forest_edge_density gives the edge length per hectare, in nrv_metrics_landscape's format", {
  d <- withr::local_tempdir()
  f <- file.path(d, "rep01", "forestMap_year0000.tif")
  dir.create(dirname(f))
  m <- forest_nonforest_map(.fnf_cd(), .fnf_halves())
  terra::writeRaster(m, f, datatype = "INT1U")
  polys <- sf::st_as_sf(data.frame(Name = c("all", "left", "right")), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)))),
    sf::st_polygon(list(rbind(c(0, 0), c(20, 0), c(20, 40), c(0, 40), c(0, 0)))),
    sf::st_polygon(list(rbind(c(20, 0), c(40, 0), c(40, 40), c(20, 40), c(20, 0)))),
    crs = "EPSG:32611"
  ))

  out <- nrv_forest_edge_density(f, polys, "Name")

  ref <- nrv_metrics_landscape(polys, "Name", f, funList = list("lsm_l_ed"))[[1]]
  expect_named(out, names(ref))
  expect_equal(out$level, rep("landscape", 3))
  expect_equal(out$metric, rep("forest_ed", 3))
  expect_equal(out$rep, rep(1L, 3))
  expect_equal(out$time, rep(0L, 3))
  ## one 40 m edge over 0.16 ha is 250 m/ha; a polygon with only forest or only non-forest has none
  v <- stats::setNames(out$value, out$poly)
  expect_equal(v[["all"]], 250)
  expect_equal(v[["left"]], 0)
  expect_equal(v[["right"]], 0)
  ## and it is the landscape's own edge density, as both classes share the one edge
  expect_equal(ref$value[ref$poly == "all"], 250)
})

test_that("nrv_forest_edge_density sums every forest edge", {
  d <- withr::local_tempdir()
  f <- file.path(d, "rep02", "forestMap_year0100.tif")
  dir.create(dirname(f))
  ## one forest pixel inside non-forest (4 x 10 m of edge) and one 2-pixel forest strip on the border
  pgm <- .fnf_pgm(c(2, 2, 2, 2,  2, 1, 2, 2,  2, 2, 2, 2,  1, 1, 2, 2))
  terra::writeRaster(forest_nonforest_map(.fnf_cd(), pgm), f, datatype = "INT1U")
  polys <- sf::st_as_sf(data.frame(Name = "all"), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)))), crs = "EPSG:32611"))
  out <- nrv_forest_edge_density(f, polys, "Name")
  ## edges inside the map: 40 m around the pixel + 20 m above the strip + 10 m right of it (the map
  ## border is no edge)
  expect_equal(out$value, 70 / 0.16)
  expect_equal(c(out$rep, out$time), c(2L, 100L))
})

test_that("nrv_metric_info names forest edge density", {
  expect_equal(nrv_metric_info("forest_ed", "landscape")$label, "Forest edge density (m/ha)")
})
