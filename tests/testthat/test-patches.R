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

testthat::test_that("patchAreasSeral returns empty (not error) for an all-NA / RAT-less subregion", {
  ## an empty crop has no category table -> the pre-guard code errored in .rat_value_col()
  ssm <- terra::rast(matrix(NA_real_, nrow = 5, ncol = 5), crs = "EPSG:3857")
  areas <- patchAreasSeral(ssm)
  testthat::expect_equal(nrow(areas), 0L)
  testthat::expect_named(areas, c("layer", "level", "class", "id", "metric", "value"))
})

testthat::test_that("patchStatsSeral guards a subregion with no flammable pixels", {
  dir <- withr::local_tempdir()
  ssm <- .create_mock_vtm()
  levels(ssm) <- data.frame(ID = 1:3, values = c("Early", "Mid", "Late"))
  flm <- terra::rast(matrix(0, nrow = 5, ncol = 5), crs = "EPSG:3857") ## all non-flammable
  terra::ext(flm) <- terra::ext(ssm)
  ssm_f <- file.path(dir, "seralStageMap_year0.tif")
  flm_f <- file.path(dir, "flam.tif")
  terra::writeRaster(ssm, ssm_f)
  terra::writeRaster(flm, flm_f)
  poly <- terra::as.polygons(terra::ext(ssm), crs = terra::crs(ssm))
  poly$Name <- "Z1"

  ## every pixel masked non-flammable -> all-NA crop -> guard returns empty tables, no error
  res <- patchStatsSeral(ssm_f, flm_f, "Z1", poly, "Name", c("patchAreasSeral"))
  inner <- res[[1L]]
  testthat::expect_named(inner, "patchAreasSeral")
  testthat::expect_equal(nrow(inner[["patchAreasSeral"]]), 0L)
})

testthat::test_that("patchStatsSeral relabels class-level lsm_c_* codes with seral stage names", {
  dir <- withr::local_tempdir()
  ssm <- .create_mock_vtm()
  levels(ssm) <- data.frame(ID = 1:3, values = c("Early", "Mid", "Late"))
  flm <- terra::rast(matrix(1, nrow = 5, ncol = 5), crs = "EPSG:3857") ## all flammable
  terra::ext(flm) <- terra::ext(ssm)
  ssm_f <- file.path(dir, "seralStageMap_year0.tif")
  flm_f <- file.path(dir, "flam.tif")
  terra::writeRaster(ssm, ssm_f)
  terra::writeRaster(flm, flm_f)
  poly <- terra::as.polygons(terra::ext(ssm), crs = terra::crs(ssm))
  poly$Name <- "Z1"

  res <- patchStatsSeral(ssm_f, flm_f, "Z1", poly, "Name", c("lsm_c_ca", "patchAreasSeral"))[[1L]]

  ## lsm_c_ca reports the raw integer category; it must come back as the stage name
  testthat::expect_setequal(res[["lsm_c_ca"]]$class, c("Early", "Mid", "Late"))
  ## patchAreasSeral already labels its classes and is unchanged
  testthat::expect_setequal(res[["patchAreasSeral"]]$class, c("Early", "Mid", "Late"))
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

testthat::test_that("label_rat_classes() relabels from any categorical map's RAT", {
  ssm <- terra::rast(matrix(c(1, 5, 9, 13), nrow = 2), crs = "EPSG:3857")
  levels(ssm) <- data.frame(value = c(1, 5, 9, 13), seral = c("early", "mid", "mature", "old"))

  df <- data.frame(class = c(1, 5, 9, 13), value = 1:4)
  testthat::expect_equal(label_rat_classes(df, ssm)$class, c("early", "mid", "mature", "old"))
  ## label_vegtype_classes() is the vegtype-facing spelling of the same operation
  vtm <- .create_mock_vtm()
  testthat::expect_equal(
    label_vegtype_classes(data.frame(class = 1:3), vtm)$class,
    label_rat_classes(data.frame(class = 1:3), vtm)$class
  )
})

testthat::test_that(".parse_metric_labels() handles poly names containing '_' and '.'", {
  labels <- c(
    "rep01.vegTypeMap_year0000_SBSmc2",
    "rep02.seralStageMap_year1200_NDT3_SBS",
    "rep10.vegTypeMap_year0050_Big Creek",
    "rep03.vegTypeMap_year0100_Mt. Tom_West"
  )
  out <- .parse_metric_labels(labels)

  testthat::expect_equal(out$rep, c(1L, 2L, 10L, 3L))
  testthat::expect_equal(out$time, c(0L, 1200L, 50L, 100L))
  testthat::expect_equal(out$poly, c("SBSmc2", "NDT3_SBS", "Big Creek", "Mt. Tom_West"))
})

testthat::test_that(".parse_metric_labels() errors informatively on an unparseable label", {
  testthat::expect_snapshot(error = TRUE, .parse_metric_labels("nonsense"))
})

testthat::test_that("subregion_forested_area() tabulates ha per subregion x species", {
  vtm <- .create_mock_vtm() ## 5x5: forest=8, grass=8, water=9 cells (all classified)
  cell_ha <- prod(terra::res(vtm)) / 1e4
  poly <- terra::as.polygons(terra::ext(vtm), crs = terra::crs(vtm))
  poly$Name <- "Z1"

  a <- subregion_forested_area(vtm, poly, "Name")
  get <- function(sp) a$area_ha[a$poly == "Z1" & a$vegCover == sp]

  testthat::expect_equal(get("forest"), 8 * cell_ha)
  testthat::expect_equal(get("grass"), 8 * cell_ha)
  testthat::expect_equal(get("water"), 9 * cell_ha)
  ## the "All species" row is the subregion total
  testthat::expect_equal(get("All species"), 25 * cell_ha)
  ## all_label = NULL drops the totals row
  testthat::expect_false(
    "All species" %in% subregion_forested_area(vtm, poly, "Name", all_label = NULL)$vegCover
  )
})

testthat::test_that(".get_fun() resolves bare names and namespaced pkg::fun funList entries (#1)", {
  ## bare name -> looked up as before
  testthat::expect_identical(.get_fun("patchAreasSeral"), patchAreasSeral)
  ## explicit pkg::fun -> resolved from that package's namespace
  testthat::expect_identical(.get_fun("landscapemetrics::lsm_l_ta"), landscapemetrics::lsm_l_ta)
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
