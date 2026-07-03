testthat::test_that("plot_over_time works correctly", {
  summary_df <- data.frame(
    time = rep(2000:2002, 2),
    mean = stats::runif(6, 10, 20),
    sd = stats::runif(6, 1, 2),
    poly = rep(c("polyA", "polyB"), each = 3)
  )

  p <- plot_over_time(summary_df, "My Y-axis Label")

  ## Test if the output is a ggplot object
  expect_s3_class(p, "ggplot")

  ## Test that it doesn't throw an error
  expect_no_error(print(p))
})

testthat::test_that("plot_over_time_by_class works correctly", {
  summary_df <- data.frame(
    time = rep(2000:2002, 4),
    mean = stats::runif(12, 10, 20),
    sd = stats::runif(12, 1, 2),
    poly = rep(c("polyA", "polyB"), each = 6),
    class = rep(c("class1", "class2"), 6)
  )

  p <- plot_over_time_by_class(summary_df, "My Y-axis Label")

  ## Test if the output is a ggplot object
  testthat::expect_s3_class(p, "ggplot")

  ## Test that it doesn't throw an error
  testthat::expect_no_error(print(p))
})

testthat::test_that("plot_by_class works correctly", {
  summary_df <- data.frame(
    mean = stats::runif(20, 10, 20),
    poly = rep(c("polyA", "polyB"), each = 10),
    class = rep(paste0("class", 1:5), 4),
    metric = "test_metric"
  )

  ## Test with type = "box"
  p_box <- plot_by_class(summary_df, type = "box")
  testthat::expect_s3_class(p_box, "ggplot")
  testthat::expect_no_error(print(p_box))

  ## Test with type = "violin"
  p_violin <- plot_by_class(summary_df, type = "violin")
  testthat::expect_s3_class(p_violin, "ggplot")
  testthat::expect_no_error(print(p_violin))

  ## Test that it throws an error with an invalid type
  testthat::expect_error(plot_by_class(summary_df, type = "invalid_type"))
})

testthat::test_that("plot_nrv_envelope facets varying columns and handles empty input", {
  env <- data.frame(
    time = rep(c(0, 50), 4),
    poly = rep(c("polyA", "polyB"), each = 4),
    metric = rep(c("m1", "m2"), 4),
    mean = stats::runif(8, 10, 20),
    min = stats::runif(8, 5, 10),
    max = stats::runif(8, 20, 25)
  )

  p <- plot_nrv_envelope(env)
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_no_error(print(p))

  testthat::expect_null(plot_nrv_envelope(env[0, ]))
  testthat::expect_null(plot_nrv_envelope(NULL))
})
