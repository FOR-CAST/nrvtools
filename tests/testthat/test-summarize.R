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
    c(
      "time",
      "poly",
      "metric",
      "n_reps",
      "mean",
      "sd",
      "min",
      "q25",
      "median",
      "q75",
      "max",
      "se",
      "ci"
    )
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

## one replicate: two summary years of a patch-level metric (3 then 2 patches) and a class-level one
new_replicate <- function(r) {
  rbind(
    data.frame(
      time = c(0, 0, 0, 50, 50),
      poly = "A",
      class = "pine",
      level = "patch",
      metric = "area",
      value = c(1, 2, 3, 10, 20) * r
    ),
    data.frame(
      time = c(0, 50),
      poly = "A",
      class = "pine",
      level = "class",
      metric = "ca",
      value = c(6, 30) * r
    )
  )
}

test_that("nrv_replicate_values collapses patches to a per-replicate mean", {
  root <- withr::local_tempdir()
  for (r in 1:2) {
    write_nrv_parquet(new_replicate(r), root, r)
  }
  v <- nrv_replicate_values(root)
  v <- v[order(v$metric, v$replicate, v$time), ]

  expect_named(v, c("replicate", "time", "poly", "class", "level", "metric", "n", "value"))
  expect_equal(nrow(v), 8L) ## 2 replicates x 2 years x 2 metrics
  area <- v[v$metric == "area", ]
  expect_equal(area$value, c(2, 15, 4, 30))
  expect_equal(area$n, c(3, 2, 3, 2))
  ca <- v[v$metric == "ca", ]
  expect_equal(ca$value, c(6, 30, 12, 60))
  expect_equal(ca$n, rep(1, 4))
})

test_that("nrv_replicate_values reads a data.frame the same way", {
  df <- new_replicate(1)
  df$replicate <- 1L
  v <- nrv_replicate_values(df)
  expect_equal(v$value[v$metric == "area"], c(2, 15))
  expect_equal(nrow(nrv_replicate_values(df, id_cols = "metric")), 2L)
})

test_that("nrv_replicate_values returns an empty frame without data", {
  expect_equal(nrow(nrv_replicate_values(withr::local_tempdir())), 0L)
})

test_that("nrv_replicate_values errors on a missing value column", {
  expect_snapshot(nrv_replicate_values(new_replicate(1), value_col = "nope"), error = TRUE)
})

test_that("summarize_nrv gives exact type 7 quartiles", {
  root <- withr::local_tempdir()
  withr::local_seed(2)
  vals <- lapply(1:7, function(r) {
    ## whole numbers, so values tie within and across replicates; missing values are skipped
    df <- data.frame(
      time = rep(c(0, 50), each = 3),
      poly = "A",
      value = round(stats::runif(6, 0, 9))
    )
    if (r == 1L) {
      df$value[c(1, 4)] <- c(NA, NaN)
    }
    write_nrv_parquet(df, root, r)
    df
  })
  env <- summarize_nrv(root)
  ov <- do.call(rbind, vals)
  for (t in c(0, 50)) {
    v <- ov$value[ov$time == t]
    expect_identical(
      unlist(env[env$time == t, c("q25", "median", "q75")], use.names = FALSE),
      unname(stats::quantile(v, c(0.25, 0.5, 0.75), na.rm = TRUE, type = 7))
    )
  }
})

test_that("summarize_nrv takes the per-replicate values of nrv_replicate_values()", {
  root <- withr::local_tempdir()
  for (r in 1:2) {
    write_nrv_parquet(new_replicate(r), root, r)
  }
  env <- summarize_nrv(nrv_replicate_values(root))
  area <- env[env$metric == "area", ]
  expect_equal(area$n_reps, c(2L, 2L)) ## two replicates a year, not five patches
  expect_equal(area$mean, c(3, 22.5))
  expect_equal(area$median, c(3, 22.5))
  expect_equal(nrow(summarize_nrv(data.frame())), 0L)
})

## one replicate of a patch-level metric holding NA and NaN: class p has a NaN among its values, q
## only NA, r only NaN, s one NA
new_nan_replicate <- function() {
  data.frame(
    time = 0,
    poly = "A",
    class = c("p", "p", "p", "s", "s", "q", "q", "r"),
    level = "patch",
    metric = "area",
    value = c(1, 2, NaN, NA, 4, NA, NA, NaN)
  )
}

test_that("NaN is missing in Arrow as in a data.frame", {
  root <- withr::local_tempdir()
  write_nrv_parquet(new_nan_replicate(), root, 1L)
  df <- new_nan_replicate()
  df$replicate <- 1L

  v <- nrv_replicate_values(root)
  expect_equal(nrv_replicate_values(df), v)
  expect_equal(v$class, c("p", "q", "r", "s"))
  expect_equal(v$value, c(1.5, NA, NA, 4))
  expect_equal(v$n, c(3L, 2L, 1L, 2L))

  env <- summarize_nrv(root)
  expect_equal(summarize_nrv(df), env)
  expect_equal(env$mean, c(1.5, NA, NA, 4))
  expect_equal(env$min, c(1, NA, NA, 4))
  expect_equal(env$median, c(1.5, NA, NA, 4))
})

test_that("rows come back sorted by the id columns", {
  root <- withr::local_tempdir()
  for (r in c(3L, 1L, 2L)) {
    write_nrv_parquet(data.frame(time = c(50, 0), poly = c("B", "A"), value = r), root, r)
  }
  v <- nrv_replicate_values(root)
  expect_equal(v$replicate, rep(1:3, each = 2))
  expect_equal(v$time, rep(c(0, 50), 3))
  expect_equal(rownames(v), as.character(1:6))
  expect_identical(nrv_replicate_values(root), v)

  env <- summarize_nrv(root)
  expect_equal(env$time, c(0, 50))
  expect_equal(env$poly, c("A", "B"))
  expect_identical(summarize_nrv(root), env)
})

test_that("per-replicate means do not depend on the order of the rows", {
  ## patch areas of 1.44 ha cells, which binary floating point holds inexactly, so the order in
  ## which a group's values are added up can change the last digits of their sum
  withr::local_seed(3)
  df <- data.frame(
    replicate = 1L,
    time = 0,
    poly = "A",
    class = rep(sprintf("c%02d", 1:20), each = 200),
    level = "patch",
    metric = "area",
    value = 1.44 * sample.int(400, 4000, replace = TRUE)
  )
  v <- nrv_replicate_values(df)
  expect_equal(v$value, as.vector(tapply(df$value, df$class, mean)))
  for (i in 1:3) {
    expect_identical(nrv_replicate_values(df[sample.int(nrow(df)), ]), v)
  }
  ## and Arrow, reading the rows in yet another order, gives the same numbers
  root <- withr::local_tempdir()
  write_nrv_parquet(df[sample.int(nrow(df)), ], root, 1L)
  expect_identical(nrv_replicate_values(root), v)
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
