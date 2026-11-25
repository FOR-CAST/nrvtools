## 4 x 4 cells of 10 m (1600 m2 = 0.16 ha); the maps are stand ages in years
.fnf_age <- function(vals) {
  terra::rast(matrix(vals, 4, 4, byrow = TRUE), crs = "EPSG:32611", extent = terra::ext(0, 40, 0, 40))
}
## left half 100 years (forest), right half 10 years (non-forest): one 40 m edge
.fnf_halves <- function() .fnf_age(rep(c(100, 100, 10, 10), 4))

test_that("forest_nonforest_map classifies by stand age", {
  age <- .fnf_age(c(100, 10, 55, 19,  NA, 20, NA, 1,  100, 100, 100, 100,  10, 10, 10, 10))
  m <- forest_nonforest_map(age)
  expect_s4_class(m, "SpatRaster")
  ## 20 years is forest; no age without a study area is outside it
  expect_equal(
    as.vector(terra::values(m)),
    c(1, 0, 1, 0,  NA, 1, NA, 0,  1, 1, 1, 1,  0, 0, 0, 0)
  )
  expect_equal(terra::levels(m)[[1]]$values, c("Non-forest", "Forest"))
  expect_equal(as.vector(terra::values(forest_nonforest_map(age, ageThreshold = 19)))[1:4], c(1, 0, 1, 1))
  expect_equal(as.vector(terra::values(forest_nonforest_map(age, ageThreshold = 60)))[1:4], c(1, 0, 0, 0))
})

test_that("forest_nonforest_map honours the non-forest mask and the study area", {
  age <- .fnf_halves()
  nf <- terra::rast(age, vals = rep(c(0, 1, 0, 0), 4)) ## column 2 is non-forest land cover
  m <- forest_nonforest_map(age, nonForest = nf)
  expect_equal(as.vector(terra::values(m))[1:4], c(1, 0, 0, 0))
  ## a study area of the left three columns: outside is NA; an NA age inside it (no cohorts) is non-forest
  age2 <- .fnf_age(c(100, NA, 10, 10, rep(c(100, 100, 10, 10), 3)))
  area <- terra::as.polygons(terra::ext(0, 30, 0, 40), crs = "EPSG:32611")
  m2 <- forest_nonforest_map(age2, studyArea = area)
  expect_equal(as.vector(terra::values(m2))[1:4], c(1, 0, 0, NA))
  ## in another CRS, the same
  m3 <- forest_nonforest_map(age2, studyArea = terra::project(area, "EPSG:4326"))
  expect_equal(as.vector(terra::values(m3))[1:4], c(1, 0, 0, NA))
})

test_that("forest_nonforest_map reads the age map from a file", {
  d <- withr::local_tempdir()
  terra::writeRaster(.fnf_halves(), file.path(d, "age.tif"))
  expect_equal(
    as.vector(terra::values(forest_nonforest_map(file.path(d, "age.tif")))),
    as.vector(terra::values(forest_nonforest_map(.fnf_halves())))
  )
})

test_that("nrv_forest_edge_density gives the edge length per hectare, in nrv_metrics_landscape's format", {
  d <- withr::local_tempdir()
  f <- file.path(d, "rep01", "forestMap_year0000.tif")
  dir.create(dirname(f))
  m <- forest_nonforest_map(.fnf_halves())
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
  age <- .fnf_age(c(10, 10, 10, 10,  10, 100, 10, 10,  10, 10, 10, 10,  100, 100, 10, 10))
  terra::writeRaster(forest_nonforest_map(age), f, datatype = "INT1U")
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
