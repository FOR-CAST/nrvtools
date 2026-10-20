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
    q25 = stats::runif(8, 10, 12),
    median = stats::runif(8, 12, 15),
    q75 = stats::runif(8, 15, 18),
    max = stats::runif(8, 20, 25)
  )

  p <- plot_nrv_envelope(env)
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_no_error(print(p))

  testthat::expect_null(plot_nrv_envelope(env[0, ]))
  testthat::expect_null(plot_nrv_envelope(NULL))
})

testthat::test_that("plot_nrv_envelope panels follow the facet columns' factor ordering", {
  ## seral stages sort alphabetically to early, mature, mid, old -- a factor `class` column must
  ## instead panel in its own (ecological) order.
  stages <- c("early", "mid", "mature", "old")
  env <- data.frame(
    time = rep(c(0, 50), each = 4),
    class = factor(rep(stages, 2), levels = stages),
    mean = 1:8,
    min = 1:8,
    q25 = 1:8,
    median = 1:8,
    q75 = 1:8,
    max = 1:8
  )

  p <- plot_nrv_envelope(env)
  testthat::expect_equal(levels(p$data$.panel), stages)

  ## a plain character column still sorts naturally
  env$class <- as.character(env$class)
  testthat::expect_equal(levels(plot_nrv_envelope(env)$data$.panel), sort(stages))
})

testthat::test_that("plot_nrv_envelope draws a box-and-whisker from the five-number summary", {
  env <- data.frame(
    time = rep(c(0, 50), 2),
    poly = rep(c("polyA", "polyB"), each = 2),
    mean = stats::runif(4, 10, 20),
    min = stats::runif(4, 5, 10),
    q25 = stats::runif(4, 10, 12),
    median = stats::runif(4, 12, 15),
    q75 = stats::runif(4, 15, 18),
    max = stats::runif(4, 20, 25)
  )

  p <- plot_nrv_envelope(env, type = "boxplot")
  testthat::expect_s3_class(p, "ggplot")
  testthat::expect_no_error(print(p))

  ## boxplot needs the five-number summary
  testthat::expect_snapshot(
    plot_nrv_envelope(env[, c("time", "poly", "mean", "min", "max")], type = "boxplot"),
    error = TRUE
  )
})

## a landscape-metric envelope over two subregions and two summary years
new_envelope <- function(polys = c("A", "B")) {
  data.frame(
    time = rep(c(700, 800), length(polys)),
    poly = rep(polys, each = 2),
    class = NA,
    level = "landscape",
    metric = "ed",
    mean = rep(c(10, 11), length(polys)),
    min = 8,
    q25 = 9,
    median = rep(c(10, 11), length(polys)),
    q75 = 12,
    max = 13
  )
}

## its current-condition envelope, with a subregion the simulated envelope lacks
new_cc_envelope <- function() {
  data.frame(
    time = 0,
    poly = c("A", "B", "C"),
    class = NA,
    level = "landscape",
    metric = "ed",
    mean = c(15, 25, 99),
    median = c(14, 24, 98)
  )
}

geom_data <- function(p, geom) {
  l <- Filter(function(l) inherits(l$geom, geom), p$layers)
  if (length(l)) as.data.frame(l[[1]]$data) else NULL
}

test_that("plot_nrv_envelope draws the current condition once per panel", {
  p <- plot_nrv_envelope(new_envelope(), cc = new_cc_envelope())
  cc <- geom_data(p, "GeomHline")

  expect_equal(as.character(cc$.panel), c("A", "B"))
  expect_equal(cc$.cc, c(15, 25))
  expect_equal(levels(p$data$.panel), c("A", "B")) ## no panel for C
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_nrv_envelope compares the current-condition median with the boxes", {
  p <- plot_nrv_envelope(new_envelope(), type = "boxplot", cc = new_cc_envelope())
  expect_equal(geom_data(p, "GeomHline")$.cc, c(14, 24))

  p <- plot_nrv_envelope(new_envelope(), type = "boxplot", cc = new_cc_envelope(), cc_col = "mean")
  expect_equal(geom_data(p, "GeomHline")$.cc, c(15, 25))
})

test_that("plot_nrv_envelope matches current conditions on units that do not vary", {
  ## one subregion -> one plain panel; the other subregions' values must not be averaged in
  p <- plot_nrv_envelope(new_envelope("B"), cc = new_cc_envelope())
  expect_equal(geom_data(p, "GeomHline")$.cc, 25)
})

test_that("plot_nrv_envelope applies a current condition without unit columns to every panel", {
  p <- plot_nrv_envelope(new_envelope(), cc = data.frame(value = 5))
  expect_equal(geom_data(p, "GeomHline")$.cc, c(5, 5))
})

test_that("plot_nrv_envelope draws no line without a current-condition value", {
  cc <- new_cc_envelope()
  cc$mean <- NA_real_
  expect_null(geom_data(plot_nrv_envelope(new_envelope()), "GeomHline"))
  expect_null(geom_data(plot_nrv_envelope(new_envelope(), cc = cc), "GeomHline"))
  expect_null(geom_data(plot_nrv_envelope(new_envelope(), cc = cc[0, ]), "GeomHline"))
})

test_that("plot_nrv_envelope paginates with the current-condition line", {
  polys <- sprintf("P%02d", 1:13)
  cc <- data.frame(poly = polys, class = NA, level = "landscape", metric = "ed", mean = 1:13)
  p <- plot_nrv_envelope(new_envelope(polys), cc = cc, page = 2)
  expect_equal(ggforce::n_pages(p), 2)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_nrv_envelope errors on a current condition without a value column", {
  expect_snapshot(plot_nrv_envelope(new_envelope(), cc = data.frame(poly = "A")), error = TRUE)
})

test_that("the boxplot falls back to the current-condition mean", {
  cc <- new_cc_envelope()
  cc$median <- NULL
  p <- plot_nrv_envelope(new_envelope(), type = "boxplot", cc = cc)
  expect_equal(geom_data(p, "GeomHline")$.cc, c(15, 25))
})

test_that("a current condition that matches no panel warns and draws nothing", {
  cc <- new_cc_envelope()
  cc$poly <- tolower(cc$poly)
  expect_warning(p <- plot_nrv_envelope(new_envelope(), cc = cc), "no current-condition row")
  expect_null(geom_data(p, "GeomHline"))
})

test_that("several current-condition rows in one panel warn, and their mean is drawn", {
  cc <- new_cc_envelope()
  cc <- rbind(cc, transform(cc, mean = mean + 10))
  expect_warning(p <- plot_nrv_envelope(new_envelope(), cc = cc), "2 current-condition value")
  expect_equal(geom_data(p, "GeomHline")$.cc, c(20, 30))
})

test_that("plot_nrv_envelope keeps its axis titles and takes an x title and caption", {
  b <- ggplot2::ggplot_build(plot_nrv_envelope(new_envelope()))
  expect_equal(b$plot$labels$x, "time")
  expect_equal(b$plot$labels$y, "value")
  expect_null(b$plot$labels$caption)

  p <- plot_nrv_envelope(new_envelope(), xlab = "Simulation year", caption = "Grey band: range")
  b <- ggplot2::ggplot_build(p)
  expect_equal(b$plot$labels$x, "Simulation year")
  expect_equal(b$plot$labels$caption, "Grey band: range")
})

test_that("plot_nrv_envelope labels its y axis with thousands separators", {
  env <- new_envelope()
  cols <- c("mean", "min", "q25", "median", "q75", "max")
  env[cols] <- env[cols] * 1e5
  for (type in c("ribbon", "boxplot")) {
    b <- ggplot2::ggplot_build(plot_nrv_envelope(env, type))
    lab <- b$layout$panel_params[[1]]$y$get_labels()
    expect_true(any(grepl(",", lab)))
    expect_false(any(grepl("e[+]", lab)))
  }
})

test_that("plot_nrv_envelope sets the grid of an unpaginated figure", {
  env <- new_envelope(c("A", "B", "C"))
  expect_equal(max(ggplot2::ggplot_build(plot_nrv_envelope(env))$layout$layout$ROW), 1)
  expect_equal(max(ggplot2::ggplot_build(plot_nrv_envelope(env, ncol = 1))$layout$layout$ROW), 3)
  p <- plot_nrv_envelope(new_envelope(sprintf("P%02d", 1:13)), page = 1)
  expect_equal(ggforce::n_pages(p), 2) ## 4 x 3 to a page by default
})

## per-replicate values of one class-level metric: three replicates, two subregions, two classes
new_replicate_values <- function() {
  data.frame(
    replicate = rep(1:3, each = 4),
    time = 700,
    poly = rep(c("A", "A", "B", "B"), 3),
    class = rep(c("pine", "spruce"), 6),
    level = "class",
    metric = "ca",
    value = 1:12
  )
}

test_that("plot_by_class keeps its defaults", {
  summary_df <- data.frame(
    mean = 1:4,
    poly = "polyA",
    class = c("class1", "class2"),
    metric = "test_metric"
  )
  p <- plot_by_class(summary_df)
  expect_s3_class(p, "ggplot")
  expect_equal(ggforce::n_pages(p), 1)
  expect_null(plot_by_class(summary_df[0, ]))
})

test_that("plot_by_class draws current conditions per class and panel", {
  cc <- data.frame(
    poly = c("A", "A", "B", "C"),
    class = c("pine", "fir", "spruce", "pine"),
    level = "class",
    metric = "ca",
    value = c(100, 200, 300, 400)
  )
  p <- plot_by_class(new_replicate_values(), value_col = "value", cc = cc)
  pts <- geom_data(p, "GeomPoint")
  pts <- pts[order(pts$.panel, pts$class), ]

  ## C has no panel; fir has no simulated value in A, but keeps its point
  expect_equal(as.character(pts$.panel), c("A", "A", "B"))
  expect_equal(as.character(pts$class), c("pine", "fir", "spruce"))
  expect_equal(pts$.cc, c(100, 200, 300))
  expect_equal(levels(p$data$class), c("pine", "spruce", "fir"))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_by_class drops values that are not finite, with a message", {
  reps <- new_replicate_values()
  reps$value[1:2] <- c(NaN, NA)
  expect_message(p <- plot_by_class(reps, value_col = "value"), "dropped 2 row")
  expect_equal(nrow(p$data), 10L)
  expect_no_warning(ggplot2::ggplot_build(p))

  reps$value <- NA_real_
  expect_null(plot_by_class(reps, value_col = "value"))
})

## nrvtools 0.3.4's plot_by_class(): a call it accepted must draw the same figure
plot_by_class_0.3.4 <- function(summary_df, type = c("box", "violin"), page = 1) {
  stopifnot(type %in% c("box", "violin"))
  ggplot2::ggplot(summary_df, ggplot2::aes(x = class, y = mean)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    switch(
      type,
      box = ggplot2::geom_boxplot(outlier.colour = "grey4", outlier.shape = 21, outlier.size = 1.0),
      violin = ggplot2::geom_violin()
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 14)) +
    ggplot2::ylab(summary_df$metric)
}

## what a by-class figure draws: its panels, each layer's data, the classes and the axis titles
drawn <- function(p) {
  b <- ggplot2::ggplot_build(p)
  lay <- as.data.frame(b$layout$layout)
  list(
    panels = data.frame(
      PANEL = lay$PANEL,
      ROW = lay$ROW,
      COL = lay$COL,
      poly = as.character(lay$poly)
    ),
    data = lapply(b$data, as.data.frame),
    classes = b$layout$panel_params[[1]]$x$get_labels(),
    x = b$plot$labels$x,
    y = unique(b$plot$labels$y)
  )
}

## an envelope of one class-level metric: `npoly` subregions x three classes x three summary years
new_class_envelope <- function(npoly = 3) {
  withr::local_seed(4)
  d <- expand.grid(
    time = c(700, 800, 900),
    poly = sprintf("P%02d", seq_len(npoly)),
    class = c("Decid", "Mixed", "Pine"),
    stringsAsFactors = FALSE
  )
  d$level <- "class"
  d$metric <- "ca"
  d$mean <- round(stats::runif(nrow(d), 10, 100), 2)
  d
}

test_that("plot_by_class draws what 0.3.4 drew for the same call", {
  for (type in c("box", "violin")) {
    d <- new_class_envelope()
    expect_equal(drawn(plot_by_class(d, type)), drawn(plot_by_class_0.3.4(d, type)))
    d <- new_class_envelope(13)
    for (page in 1:2) {
      expect_equal(
        drawn(plot_by_class(d, type, page = page)),
        drawn(plot_by_class_0.3.4(d, type, page = page))
      )
    }
  }
})

test_that("plot_by_class keeps the panels and classes of values that are not finite", {
  d <- new_class_envelope()
  d$mean[d$poly == "P02"] <- NA
  d$mean[d$class == "Mixed"] <- NaN
  expect_message(p <- plot_by_class(d), "dropped 15 row")
  new <- drawn(p)
  old <- suppressWarnings(drawn(plot_by_class_0.3.4(d, "box")))

  expect_equal(new$panels$poly, c("P01", "P02", "P03")) ## P02 is an empty panel
  expect_equal(new$classes, c("Decid", "Mixed", "Pine")) ## Mixed keeps its place
  expect_equal(new$panels, old$panels)
  expect_equal(new$classes, old$classes)
})

test_that("plot_by_class draws a page counted from every subregion of its input", {
  d <- new_class_envelope(13)
  d$mean[d$poly == "P13"] <- NA ## the only panel of page 2
  p <- suppressMessages(plot_by_class(d, page = 2))
  expect_equal(ggforce::n_pages(p), 2)
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_by_class has no y title without a metric column, as in 0.3.4", {
  d <- new_class_envelope()
  d$metric <- NULL
  expect_null(drawn(plot_by_class(d))$y)
  expect_equal(drawn(plot_by_class(d))$y, drawn(plot_by_class_0.3.4(d, "box"))$y)
})

test_that("a layer added to plot_by_class with its own data lands in its own panel", {
  extra <- data.frame(poly = c("P01", "P02", "P03"), class = "Pine", mean = c(500, 600, 700))
  p <- plot_by_class(new_class_envelope()) + ggplot2::geom_point(data = extra)
  pts <- ggplot2::ggplot_build(p)$data[[2]]
  expect_equal(as.integer(pts$PANEL), 1:3)
  expect_equal(pts$y, extra$mean)
})

test_that("plot_by_class takes a caption and labels its y axis with thousands separators", {
  d <- new_class_envelope()
  d$mean <- d$mean * 1e4
  b <- ggplot2::ggplot_build(plot_by_class(d, caption = "Boxes: quartiles"))
  expect_equal(b$plot$labels$caption, "Boxes: quartiles")
  lab <- b$layout$panel_params[[1]]$y$get_labels()
  expect_true(any(grepl(",", lab)))
  expect_false(any(grepl("e[+]", lab)))
})

test_that("plot_by_class draws every panel at once without a page", {
  p <- plot_by_class(new_replicate_values(), "violin", page = NULL, value_col = "value")
  expect_null(ggforce::n_pages(p))
  expect_no_error(ggplot2::ggplot_build(p))
})

test_that("plot_by_class errors on a missing column", {
  expect_snapshot(plot_by_class(new_replicate_values()), error = TRUE)
})
