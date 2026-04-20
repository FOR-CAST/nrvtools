## runHistsVegCover tests --------------------------------------------------

testthat::test_that("runHistsVegCover creates output directory and returns list", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runHistsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  testthat::expect_true(dir.exists(outDir))
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 1L)
})

testthat::test_that("runHistsVegCover returns data.table with filename column", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runHistsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  poly_result <- result[[1]]
  testthat::expect_s3_class(poly_result, "data.table")
  testthat::expect_true(all(c("zone", "vegCover", "ageClass", "filename") %in% names(poly_result)))
  testthat::expect_true(all(startsWith(poly_result$filename, outDir)))
})

testthat::test_that("runHistsVegCover creates per-polygon leading subdirectory", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  runHistsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  testthat::expect_true(dir.exists(file.path(outDir, "leading", "testPoly")))
})

testthat::test_that("runHistsVegCover accepts pixelRes without error", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  testthat::expect_no_error(
    runHistsVegCover(
      summaryPolys = summaryPolys,
      polyCol      = "polyName",
      vtm          = reps$vtm,
      sam          = reps$sam,
      dPath        = outDir,
      pixelRes     = c(1000, 1000)
    )
  )
})

testthat::test_that("runHistsVegCover handles multiple polygons", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_two_polys(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runHistsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  testthat::expect_length(result, 2L)
  testthat::expect_true(dir.exists(file.path(outDir, "leading", "polyA")))
  testthat::expect_true(dir.exists(file.path(outDir, "leading", "polyB")))
})

## runHistsLargePatches tests ----------------------------------------------

testthat::test_that("runHistsLargePatches creates output directory and returns list", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runHistsLargePatches(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir,
    patchSizes   = c(0)
  )

  testthat::expect_true(dir.exists(outDir))
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 1L)
})

testthat::test_that("runHistsLargePatches writes CSV with N and NCC columns", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  runHistsLargePatches(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir,
    patchSizes   = c(0)
  )

  csv_file <- file.path(outDir, "largePatches_testPoly_0.csv")
  testthat::expect_true(file.exists(csv_file))

  df <- utils::read.csv(csv_file)
  testthat::expect_true(all(c("N", "NCC", "ageClass", "vegCover", "polygonName") %in% names(df)))
})

testthat::test_that("runHistsLargePatches creates per-polygon largePatches subdirectory", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  runHistsLargePatches(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir,
    patchSizes   = c(0)
  )

  testthat::expect_true(dir.exists(file.path(outDir, "largePatches", "testPoly", "0")))
})

testthat::test_that("runHistsLargePatches respects multiple patchSizes", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  runHistsLargePatches(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir,
    patchSizes   = c(0, 100)
  )

  testthat::expect_true(file.exists(file.path(outDir, "largePatches_testPoly_0.csv")))
  testthat::expect_true(file.exists(file.path(outDir, "largePatches_testPoly_100.csv")))
})

testthat::test_that("runHistsLargePatches respects custom ageClasses", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  runHistsLargePatches(
    summaryPolys    = summaryPolys,
    polyCol         = "polyName",
    vtm             = reps$vtm,
    sam             = reps$sam,
    dPath           = outDir,
    patchSizes      = c(0),
    ageClasses      = c("Early", "Late"),
    ageClassCutOffs = c(0L, 80L)
  )

  df <- utils::read.csv(file.path(outDir, "largePatches_testPoly_0.csv"))
  testthat::expect_true(all(df$ageClass %in% c("Early", "Late")))
})
