## runBoxplotsVegCover tests -----------------------------------------------

testthat::test_that("runBoxplotsVegCover creates output directory and CSV files", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runBoxplotsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  testthat::expect_true(dir.exists(outDir))
  testthat::expect_type(result, "list")
  testthat::expect_length(result, 1L)

  csv_data <- file.path(outDir, "leading_testPoly.csv")
  testthat::expect_true(file.exists(csv_data))
  df <- utils::read.csv(csv_data)
  testthat::expect_true(all(c("zone", "vegCover", "ageClass", "proportion") %in% names(df)))

  testthat::expect_true(file.exists(file.path(outDir, "leading_boxplots_testPoly.csv")))
})

testthat::test_that("runBoxplotsVegCover returns data.table with filename column", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runBoxplotsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  poly_result <- result[[1]]
  testthat::expect_s3_class(poly_result, "data.table")
  testthat::expect_true(all(c("zone", "vegCover", "filename") %in% names(poly_result)))
  testthat::expect_true(all(startsWith(poly_result$filename, outDir)))
})

testthat::test_that("runBoxplotsVegCover handles multiple polygons", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_two_polys(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  result <- runBoxplotsVegCover(
    summaryPolys = summaryPolys,
    polyCol      = "polyName",
    vtm          = reps$vtm,
    sam          = reps$sam,
    dPath        = outDir
  )

  testthat::expect_length(result, 2L)
  testthat::expect_true(file.exists(file.path(outDir, "leading_polyA.csv")))
  testthat::expect_true(file.exists(file.path(outDir, "leading_polyB.csv")))
})

testthat::test_that("runBoxplotsVegCover accepts pixelRes without error", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  testthat::expect_no_error(
    runBoxplotsVegCover(
      summaryPolys = summaryPolys,
      polyCol      = "polyName",
      vtm          = reps$vtm,
      sam          = reps$sam,
      dPath        = outDir,
      pixelRes     = c(1000, 1000)
    )
  )
})

testthat::test_that("runBoxplotsVegCover respects custom ageClasses and ageClassCutOffs", {
  td <- withr::local_tempdir()
  reps <- .setup_reps(td)
  summaryPolys <- .make_poly(reps$vtm[[1]])
  outDir <- file.path(td, "output")

  testthat::expect_no_error(
    runBoxplotsVegCover(
      summaryPolys    = summaryPolys,
      polyCol         = "polyName",
      vtm             = reps$vtm,
      sam             = reps$sam,
      dPath           = outDir,
      ageClasses      = c("Early", "Late"),
      ageClassCutOffs = c(0L, 80L)
    )
  )

  df <- utils::read.csv(file.path(outDir, "leading_testPoly.csv"))
  testthat::expect_true(all(df$ageClass %in% c("Early", "Late")))
})
