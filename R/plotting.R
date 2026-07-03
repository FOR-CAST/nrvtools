utils::globalVariables(c(
  "class", "time"
))

#' NRV summary plots
#'
#' @param summary_df a range-of-variation summary `data.frame` from
#'   [summarize_nrv()] (uses the across-replicate `mean` and `sd`, faceted by
#'   `poly` and, for the by-class variants, coloured/grouped by `class`).
#'
#' @param ylabel character, specifying the label for the y-axis
#'
#' @param page integer, specifying the `facet_wrap_paginate` page to plot.
#'
#' @return ggplot object; invoked for side effect of plotting
#'
#' @export
#' @rdname plot_by
plot_over_time <- function(summary_df, ylabel, page = 1) {
  ggplot(summary_df, aes(x = time, y = mean)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(ylabel)
}

#' @export
#' @rdname plot_by
plot_over_time_by_class <- function(summary_df, ylabel, page = 1) {
  ggplot(summary_df, aes(x = time, y = mean, col = class)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ylab(ylabel)
}

#' @param type character, specifying one of "box" or "violin"
#'
#' @export
#' @rdname plot_by
plot_by_class <- function(summary_df, type = c("box", "violin"), page = 1) {
  stopifnot(type %in% c("box", "violin"))

  ggplot(summary_df, aes(x = class, y = mean)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    switch(
      type,
      box = geom_boxplot(outlier.colour = "grey4", outlier.shape = 21, outlier.size = 1.0),
      violin = geom_violin()
    ) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme_bw() +
    theme(strip.text.x = element_text(size = 14)) +
    ylab(summary_df$metric)
}

#' Plot a range-of-variation envelope over time
#'
#' Draws the across-replicate mean line with a min--max ribbon for an envelope
#' produced by [summarize_nrv()], faceting by whichever of the requested
#' categorical columns actually vary. Each distinct combination gets its own
#' panel, so replicate envelopes never overlay ambiguously within a panel; a
#' constant column is dropped from the panel label, and trivial data collapses to
#' a single panel.
#'
#' @param nrv_df A range-of-variation envelope `data.frame` from
#'   [summarize_nrv()] (must contain `time`, `mean`, `min`, `max`). `NULL` or a
#'   zero-row input returns `NULL`.
#' @param facet Candidate faceting columns; those present and varying are combined
#'   into the panel label (default `c("poly", "class", "metric", "metric.1")`).
#' @param ylab Y-axis label.
#'
#' @return A `ggplot` object, or `NULL` for empty input.
#'
#' @importFrom rlang .data
#' @export
plot_nrv_envelope <- function(
  nrv_df,
  facet = c("poly", "class", "metric", "metric.1"),
  ylab = "value"
) {
  if (is.null(nrv_df) || !nrow(nrv_df)) {
    return(invisible(NULL))
  }
  facet <- intersect(facet, names(nrv_df))
  facet <- facet[vapply(facet, function(cc) length(unique(nrv_df[[cc]])) > 1L, logical(1))]
  nrv_df[[".panel"]] <- if (length(facet)) {
    do.call(paste, c(nrv_df[facet], sep = " | "))
  } else {
    "all"
  }
  ggplot2::ggplot(nrv_df, ggplot2::aes(x = .data[["time"]], y = .data[["mean"]])) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data[["min"]], ymax = .data[["max"]]), alpha = 0.25) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap(stats::as.formula("~ .panel"), scales = "free_y") +
    ggplot2::labs(x = "time", y = ylab) +
    ggplot2::theme_bw()
}
