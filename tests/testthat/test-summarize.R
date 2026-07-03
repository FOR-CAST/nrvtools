test_that("write_nrv_parquet writes an atomic hive partition and stamps replicate", {
  root <- withr::local_tempdir()
  p <- write_nrv_parquet(data.frame(time = c(0, 50), poly = "A", value = c(1, 2)), root, 3L)
  expect_match(p, "replicate=3/part-0\\.parquet$")
  expect_length(list.files(root, pattern = "\\.tmp$", recursive = TRUE), 0L)
  d <- dplyr::collect(arrow::open_dataset(p))
  expect_equal(unique(d$replicate), 3L)
})

test_that("write_nrv_parquet returns NULL for empty input", {
  root <- withr::local_tempdir()
  expect_null(write_nrv_parquet(NULL, root, 1L))
  expect_null(write_nrv_parquet(data.frame(), root, 1L))
})

test_that("open_nrv_dataset opens files or roots equivalently, NULL when empty", {
  root <- withr::local_tempdir()
  for (r in 1:3) {
    write_nrv_parquet(data.frame(time = 0, poly = "A", value = r), root, r)
  }
  files <- list.files(root, pattern = "\\.parquet$", recursive = TRUE, full.names = TRUE)
  expect_equal(nrow(dplyr::collect(open_nrv_dataset(root))), 3L)
  expect_equal(nrow(dplyr::collect(open_nrv_dataset(files))), 3L)
  expect_null(open_nrv_dataset(character(0)))
  expect_null(open_nrv_dataset(withr::local_tempdir()))
})

test_that("summarize_nrv matches an in-memory oracle and derives se", {
  root <- withr::local_tempdir()
  withr::local_seed(1)
  vals <- lapply(1:5, function(r) {
    df <- data.frame(time = c(0, 50), poly = c("A", "B"), metric = "x", value = stats::runif(2))
    write_nrv_parquet(df, root, r)
    df
  })
  env <- summarize_nrv(root)
  expect_setequal(
    names(env),
    c("time", "poly", "metric", "n_reps", "mean", "sd", "min", "max", "median", "se", "ci")
  )
  expect_equal(unique(env$n_reps), 5L)
  ov <- do.call(rbind, vals)
  ov <- ov$value[ov$time == 0 & ov$poly == "A"]
  cell <- env[env$time == 0 & env$poly == "A", ]
  expect_equal(cell$mean, mean(ov))
  expect_equal(cell$min, min(ov))
  expect_equal(cell$max, max(ov))
  expect_equal(cell$se, cell$sd / sqrt(cell$n_reps))
})

test_that("summarize_nrv errors on a missing value column", {
  root <- withr::local_tempdir()
  write_nrv_parquet(data.frame(time = 0, poly = "A", value = 1), root, 1L)
  expect_snapshot(summarize_nrv(root, value_col = "nope"), error = TRUE)
})

test_that("tidy_nrv_metrics binds a metric list, drops empties, stamps ids", {
  metric_list <- list(
    lsm_l_ed = data.frame(level = "landscape", metric = "ed", value = 1:2, rep = 1L, time = 0),
    empty = data.frame(level = character(0), metric = character(0), value = numeric(0)),
    skip = NULL,
    patchAreas = data.frame(level = "patch", metric = "area", value = 3, rep = 1L, time = 0)
  )
  d <- tidy_nrv_metrics(metric_list, studyArea = "SA1", scenario = "hrv")
  expect_equal(nrow(d), 3L)
  expect_equal(names(d)[1:2], c("studyArea", "scenario"))
  expect_equal(unique(d$studyArea), "SA1")
  expect_setequal(d$metric, c("ed", "area"))
  ## a single data.frame is accepted and, with no ids, returned unstamped
  d1 <- tidy_nrv_metrics(metric_list$lsm_l_ed)
  expect_false("studyArea" %in% names(d1))
  expect_equal(nrow(d1), 2L)
})

test_that("tidy_nrv_metrics returns an empty frame when nothing to bind", {
  expect_equal(nrow(tidy_nrv_metrics(list(NULL, data.frame()))), 0L)
})

test_that("seral_stages returns the ordered BC seral classes", {
  s <- seral_stages()
  expect_type(s, "character")
  expect_equal(s[1], "early")
  expect_equal(s[length(s)], "old_Other")
  expect_length(s, 16L)
})
