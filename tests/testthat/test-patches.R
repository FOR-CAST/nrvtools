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
  levels(vtm) <- data.frame(ID = 1:3, values = c("forest", "grass", "water"))

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
  levels(ssm) <- data.frame(ID = 1:3, values = c("Early", "Mid", "Late"))
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

testthat::test_that("label_vegtype_classes() maps integer codes and leaves labels/non-matches alone", {
  vtm <- .create_mock_vtm() ## RAT: 1=forest, 2=grass, 3=water

  ## integer-coded classes (as from lsm_c_*), stored as character (post-parquet) or numeric
  df_chr <- data.frame(class = c("1", "2", "3"), value = 1:3, stringsAsFactors = FALSE)
  testthat::expect_equal(label_vegtype_classes(df_chr, vtm)$class, c("forest", "grass", "water"))
  df_num <- data.frame(class = c(1, 2, 3), value = 1:3)
  testthat::expect_equal(label_vegtype_classes(df_num, vtm)$class, c("forest", "grass", "water"))

  ## already-labelled classes (patchAges/patchAreas) and codes absent from the RAT pass through
  df_mixed <- data.frame(class = c("1", "grass", "9"), value = 1:3, stringsAsFactors = FALSE)
  testthat::expect_equal(label_vegtype_classes(df_mixed, vtm)$class, c("forest", "grass", "9"))

  ## idempotent, and a no-op for empty / class-less / RAT-less input
  once <- label_vegtype_classes(df_chr, vtm)
  testthat::expect_equal(label_vegtype_classes(once, vtm)$class, once$class)
  testthat::expect_equal(label_vegtype_classes(df_chr[0, ], vtm)$class, character(0))
  testthat::expect_named(label_vegtype_classes(data.frame(value = 1:2), vtm), "value")
})

testthat::test_that(".rat_value_col() finds the value column across RAT naming conventions", {
  ## terra RATs name the cell-value column "ID" for some rasters and "value" for others (e.g. the
  ## seral-stage map); the label column is "values". Must pick the value column, not the label.
  testthat::expect_equal(.rat_value_col(data.frame(ID = 1:2, values = c("a", "b"))), 1L)
  testthat::expect_equal(.rat_value_col(data.frame(value = 1:2, values = c("a", "b"))), 1L)
  testthat::expect_equal(.rat_value_col(data.frame(values = c("a", "b"), value = 1:2)), 2L)
  ## fall back to the first column when neither "id" nor "value" is present
  testthat::expect_equal(.rat_value_col(data.frame(foo = 1:2, bar = c("a", "b"))), 1L)
})
