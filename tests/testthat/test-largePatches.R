## largePatches ------------------------------------------------------------

testthat::test_that("largePatches returns data.table with correct columns", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = 0)

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_named(
    result,
    c("vegCover", "ageClass", "polygonName", "rep", "sizeInHa"),
    ignore.order = TRUE
  )
})

testthat::test_that("largePatches includes All_species rows", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = 0)

  testthat::expect_all_true(c("Abie_sp", "Pice_mar", "All_species") %in% result$vegCover)
})

testthat::test_that("largePatches all returned patches satisfy minPatchSize", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  min_ha <- 200
  result <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = min_ha)

  testthat::expect_all_true(result$sizeInHa >= min_ha)
})

testthat::test_that("largePatches minPatchSize=0 returns more rows than a large threshold", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result_all   <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = 0)
  result_large <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = 250)

  testthat::expect_gt(nrow(result_all), nrow(result_large))
})

testthat::test_that("largePatches rep column equals id", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- largePatches(vtm, sam, poly, "polyName", id = "rep01", minPatchSize = 0)

  testthat::expect_equal(unique(result$rep), "rep01")
})

testthat::test_that("largePatches respects custom ageClasses", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm  <- .write_vtm(file.path(td, "rep01"))
  sam  <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm)

  result <- largePatches(
    vtm, sam, poly, "polyName", id = "rep01",
    minPatchSize    = 0,
    ageClasses      = c("Early", "Late"),
    ageClassCutOffs = c(0L, 80L)
  )

  testthat::expect_all_true(result$ageClass %in% c("Early", "Late"))
})

## calculateLargePatches ---------------------------------------------------

testthat::test_that("calculateLargePatches combines results from all reps", {
  td <- withr::local_tempdir()
  for (d in c("rep01", "rep02")) dir.create(file.path(td, d))

  vtm_files <- c(.write_vtm(file.path(td, "rep01")), .write_vtm(file.path(td, "rep02")))
  sam_files <- c(.write_sam(file.path(td, "rep01")), .write_sam(file.path(td, "rep02")))
  poly <- .make_poly(vtm_files[[1]])

  result <- calculateLargePatches(poly, "polyName", vtm_files, sam_files, minPatchSize = 0)

  testthat::expect_s3_class(result, "data.table")
  testthat::expect_setequal(unique(result$rep), c("rep01", "rep02"))
})

testthat::test_that("calculateLargePatches rep names come from vtm directories", {
  td <- withr::local_tempdir()
  for (d in c("run_A", "run_B")) dir.create(file.path(td, d))

  vtm_files <- c(.write_vtm(file.path(td, "run_A")), .write_vtm(file.path(td, "run_B")))
  sam_files <- c(.write_sam(file.path(td, "run_A")), .write_sam(file.path(td, "run_B")))
  poly <- .make_poly(vtm_files[[1]])

  result <- calculateLargePatches(poly, "polyName", vtm_files, sam_files, minPatchSize = 0)

  testthat::expect_setequal(unique(result$rep), c("run_A", "run_B"))
})

testthat::test_that("calculateLargePatches minPatchSize filters combined output", {
  td <- withr::local_tempdir()
  dir.create(file.path(td, "rep01"))

  vtm_files <- .write_vtm(file.path(td, "rep01"))
  sam_files <- .write_sam(file.path(td, "rep01"))
  poly <- .make_poly(vtm_files)

  result <- calculateLargePatches(poly, "polyName", vtm_files, sam_files, minPatchSize = 250)

  testthat::expect_all_true(result$sizeInHa >= 250)
})
