## A cohortData-shaped table: non-alphabetical factor levels, one level absent from the data
cohort_fixture <- function() {
  data.table::data.table(
    speciesCode = factor(
      c("Pinu_con", "Pice_gla", "Pinu_con", "Popu_tre"),
      levels = c("Pinu_con", "Pice_gla", "Popu_tre", "Abie_bal")
    ),
    ecoregionGroup = factor(c("b", "a", "b", "a"), levels = c("b", "a")),
    pixelGroup = c(1L, 1L, 2L, 3L),
    age = c(20L, 40L, 60L, 80L),
    B = c(100L, 200L, 300L, 400L),
    mortality = c(0L, 5L, 0L, 1L),
    aNPPAct = c(1L, 2L, 3L, 4L),
    growth = c(0.5, 1.5, 2.5, 3.5)
  )
}

testthat::test_that("write_cohort_parquet() writes <root>/year=0700/part-0.parquet and returns its path", {
  root <- withr::local_tempdir()
  cd <- cohort_fixture()

  p <- write_cohort_parquet(cd, root, 700)
  testthat::expect_identical(p, file.path(root, "year=0700", "part-0.parquet"))
  testthat::expect_true(file.exists(p))
  testthat::expect_identical(
    write_cohort_parquet(cd, root, 700L),
    file.path(root, "year=0700", "part-0.parquet")
  )
  testthat::expect_identical(
    list.files(dirname(p), all.files = TRUE, no.. = TRUE),
    "part-0.parquet"
  )

  ## a second write replaces the first
  write_cohort_parquet(cd[1:2], root, 700)
  testthat::expect_equal(nrow(read_cohort_data(p)), 2L)
  testthat::expect_identical(
    list.files(dirname(p), all.files = TRUE, no.. = TRUE),
    "part-0.parquet"
  )

  ## no year column
  testthat::expect_false("year" %in% names(read_cohort_data(p)))

  ## zero rows keep their columns
  p0 <- write_cohort_parquet(cd[0], root, 5)
  testthat::expect_identical(names(read_cohort_data(p0)), names(cd))
  testthat::expect_equal(nrow(read_cohort_data(p0)), 0L)
})

testthat::test_that("a parquet round trip keeps column types, integer storage and factor levels in order", {
  root <- withr::local_tempdir()
  cd <- cohort_fixture()
  out <- read_cohort_data(write_cohort_parquet(cd, root, 100))

  testthat::expect_identical(names(out), names(cd))
  testthat::expect_identical(lapply(out, class), lapply(cd, class))
  testthat::expect_identical(vapply(out, typeof, ""), vapply(cd, typeof, ""))
  testthat::expect_identical(levels(out$speciesCode), levels(cd$speciesCode))
  testthat::expect_identical(levels(out$ecoregionGroup), levels(cd$ecoregionGroup))
  testthat::expect_equal(as.data.frame(out), as.data.frame(cd))
})

testthat::test_that("read_cohort_data() of a .qs2 equals qs2::qs_read()", {
  testthat::skip_if_not_installed("qs2")
  f <- file.path(withr::local_tempdir(), "cohortData_year0100.qs2")
  qs2::qs_save(cohort_fixture(), f)
  testthat::expect_equal(read_cohort_data(f), qs2::qs_read(f))
})

testthat::test_that("read_cohort_data(cols =) returns those columns in that order from either format", {
  testthat::skip_if_not_installed("qs2")
  tmp <- withr::local_tempdir()
  cd <- cohort_fixture()
  p <- write_cohort_parquet(cd, tmp, 100)
  q <- file.path(tmp, "cd.qs2")
  qs2::qs_save(cd, q)

  for (f in c(p, q)) {
    out <- read_cohort_data(f, cols = c("B", "pixelGroup", "speciesCode"))
    testthat::expect_identical(names(out), c("B", "pixelGroup", "speciesCode"))
    testthat::expect_equal(out$B, cd$B)
    testthat::expect_identical(levels(out$speciesCode), levels(cd$speciesCode))
    testthat::expect_identical(names(read_cohort_data(f)), names(cd))
  }
})

testthat::test_that("read_cohort_data() returns a data.table that := works on", {
  testthat::skip_if_not_installed("qs2")
  tmp <- withr::local_tempdir()
  p <- write_cohort_parquet(cohort_fixture(), tmp, 100)
  q <- file.path(tmp, "cd.qs2")
  qs2::qs_save(cohort_fixture(), q)

  for (f in c(p, q)) {
    for (cols in list(NULL, c("B", "age"))) {
      testthat::expect_visible(read_cohort_data(f, cols = cols))
      out <- read_cohort_data(f, cols = cols)
      testthat::expect_true(data.table::is.data.table(out))
      testthat::expect_no_warning(out[, newCol := B * 2L])
      testthat::expect_true("newCol" %in% names(out))
    }
  }
})

testthat::test_that("read_cohort_data() and write_cohort_parquet() reject bad input", {
  testthat::skip_if_not_installed("qs2")
  tmp <- withr::local_tempdir()
  cd <- cohort_fixture()
  p <- write_cohort_parquet(cd, tmp, 100)

  missing_f <- file.path(tmp, "nope.parquet")
  testthat::expect_error(read_cohort_data(missing_f), "nope.parquet", fixed = TRUE)
  other <- file.path(tmp, "cd.rds")
  saveRDS(cd, other)
  testthat::expect_error(read_cohort_data(other), "cd.rds", fixed = TRUE)
  testthat::expect_error(read_cohort_data(c(p, p)), "length 2", fixed = TRUE)
  testthat::expect_error(read_cohort_data(c(p, p)), basename(p), fixed = TRUE)
  testthat::expect_error(read_cohort_data(42), "42", fixed = TRUE)
  testthat::expect_error(read_cohort_data(NA_character_), "NA", fixed = TRUE)
  testthat::expect_error(
    read_cohort_data(list(a = 1, b = "x")),
    "list of length 2: <list>",
    fixed = TRUE
  )
  testthat::expect_error(
    read_cohort_data(data.frame(a = 1:2)),
    "data.frame of length 1: <data.frame>",
    fixed = TRUE
  )
  testthat::expect_error(read_cohort_data(c(1, 100)), "numeric of length 2: 1, 100", fixed = TRUE)
  testthat::expect_error(
    read_cohort_data(p, cols = c("B", "nonexistent")),
    "nonexistent",
    fixed = TRUE
  )
  q <- file.path(tmp, "cd.qs2")
  qs2::qs_save(cd, q)
  testthat::expect_error(read_cohort_data(q, cols = "nonexistent"), "nonexistent", fixed = TRUE)

  testthat::expect_error(write_cohort_parquet(cd, tmp, -1), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, 1.5), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, NA), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, NA_integer_), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, c(1, 2)), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, Inf), "year")
  testthat::expect_error(write_cohort_parquet(cd, tmp, "7"), "year")
  testthat::expect_error(write_cohort_parquet(list(1), tmp, 1), "data.frame")
})

testthat::test_that("write_cohort_parquet() stops and leaves no temp file when the rename fails", {
  root <- withr::local_tempdir()
  dst <- file.path(root, "year=0700", "part-0.parquet")
  ## a non-empty directory at the destination makes the rename fail
  dir.create(file.path(dst, "blocker"), recursive = TRUE)

  ## file.rename() also warns about the failure
  testthat::expect_error(
    suppressWarnings(write_cohort_parquet(cohort_fixture(), root, 700)),
    "failed to publish",
    fixed = TRUE
  )
  testthat::expect_length(list.files(dirname(dst), pattern = "\\.tmp$", all.files = TRUE), 0L)
})
