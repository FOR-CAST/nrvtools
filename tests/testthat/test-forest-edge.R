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

test_that("nrv_metrics_landscape(lsm_c_ed) on the map gives the forest edge length per hectare", {
  d <- withr::local_tempdir()
  edge <- function(age, rep, time) {
    f <- file.path(d, rep, sprintf("forestMap_year%04d.tif", time))
    dir.create(dirname(f), showWarnings = FALSE)
    terra::writeRaster(forest_nonforest_map(age), f, datatype = "INT1U")
    f
  }
  polys <- sf::st_as_sf(data.frame(Name = c("all", "left")), geometry = sf::st_sfc(
    sf::st_polygon(list(rbind(c(0, 0), c(40, 0), c(40, 40), c(0, 40), c(0, 0)))),
    sf::st_polygon(list(rbind(c(0, 0), c(20, 0), c(20, 40), c(0, 40), c(0, 0)))),
    crs = "EPSG:32611"
  ))
  ## one 40 m edge over 0.16 ha is 250 m/ha; a polygon with only forest has none
  out <- nrv_metrics_landscape(polys, "Name", edge(.fnf_halves(), "rep01", 0), funList = list("lsm_c_ed"))[[1]]
  forest <- out[out$class == 1, ]
  expect_equal(stats::setNames(forest$value, forest$poly), c(all = 250, left = 0))
  expect_equal(unique(forest$metric), "ed")
  ## a pixel inside non-forest (40 m) and a strip on the border (20 m above, 10 m right; the map border
  ## is no edge)
  age <- .fnf_age(c(10, 10, 10, 10,  10, 100, 10, 10,  10, 10, 10, 10,  100, 100, 10, 10))
  out <- nrv_metrics_landscape(polys[1, ], "Name", edge(age, "rep02", 100), funList = list("lsm_c_ed"))[[1]]
  expect_equal(out$value[out$class == 1], 70 / 0.16)
  expect_equal(c(unique(out$rep), unique(out$time)), c(2L, 100L))
})

test_that("nrv_metric_info names forest edge density", {
  expect_equal(nrv_metric_info("forest_ed", "landscape")$label, "Forest edge density (m/ha)")
})
