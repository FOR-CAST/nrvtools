## Helper function to create a mock categorical raster with levels
.create_mock_vtm <- function() {
  ## Create a simple matrix for the raster
  ## 1 = forest, 2 = grass, 3 = water
  m <- matrix(
    # fmt: skip
    c(1, 1, 1, 2, 2,
      1, 1, 2, 2, 3,
      1, 1, 2, 3, 3,
      1, 2, 3, 3, 3,
      2, 2, 3, 3, 3),
    nrow = 5,
    ncol = 5,
    byrow = TRUE
  )

  vtm <- terra::rast(m, crs = "EPSG:3857")

  ## Create and assign levels (attribute table)
  levels(vtm) <- data.frame(
    ID = 1:3,
    values = c("forest", "grass", "water")
  )

  return(vtm)
}

## Helper function to create a mock continuous raster
.create_mock_sam <- function() {
  m <- matrix(1:25, nrow = 5, ncol = 5, byrow = TRUE)
  sam <- terra::rast(m, crs = "EPSG:3857")
  return(sam)
}

testthat::test_that("patchAreas works correctly", {
  vtm <- .create_mock_vtm()

  areas <- patchAreas(vtm)

  ## Check output type
  testthat::expect_s3_class(areas, "tbl_df")

  ## Check column names
  testthat::expect_named(areas, c("layer", "level", "class", "id", "metric", "value"))

  ## Check that class names are correct
  testthat::expect_true(all(areas$class %in% c("forest", "grass", "water")))

  ## Check that class 0 is removed
  testthat::expect_false(0 %in% areas$class)

  ## Check values for a known patch
  ## The "forest" patch (value 1) has 8 cells
  forest_area <- areas$value[areas$class == "forest"]
  testthat::expect_equal(forest_area, 8 * prod(terra::res(vtm) * 0.01)) ## area in ha
})

testthat::test_that("patchAges works correctly", {
  vtm <- .create_mock_vtm()
  sam <- .create_mock_sam()

  ages <- patchAges(vtm, sam)

  ## Check output type
  testthat::expect_s3_class(ages, "data.table")

  ## Check column names
  testthat::expect_named(ages, c("layer", "level", "class", "id", "metric", "value"))

  ## Check that class names are correct
  testthat::expect_true(all(ages$class %in% c("forest", "grass", "water")))

  ## Check values for a known patch
  ## The "forest" patch corresponds to cells with values 1,2,3,6,7,11,12,16 in sam
  ## median(1,2,3,6,7,11,12,16) = 6.5
  forest_age <- ages$value[ages$class == "forest"]
  testthat::expect_equal(forest_age, 6.5)
})

testthat::test_that("patchAreasSeral works correctly", {
  # Can reuse the vtm helper, just pretend the classes are seral stages
  ssm <- .create_mock_vtm()
  levels(ssm) <- data.frame(
    ID = 1:3,
    values = c("Early", "Mid", "Late")
  )
  landscapemetrics::check_landscape(ssm)

  areas <- patchAreasSeral(ssm)

  # Check output type
  testthat::expect_s3_class(areas, "tbl_df")

  # Check column names
  testthat::expect_named(areas, c("layer", "level", "class", "id", "metric", "value"))

  # Check that class names are correct
  testthat::expect_true(all(areas$class %in% c("Early", "Mid", "Late")))

  # Check values for a known patch
  early_area <- areas$value[areas$class == "Early"]
  testthat::expect_equal(early_area, 8 * prod(terra::res(ssm)) / 1e4) ## areas in ha
})
