utils::globalVariables(c(
  "mn"
))

#' NRV summary plots
#'
#' @param summary_df summary `data.frame` object containing the data to use for plotting
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
  ggplot(summary_df, aes(x = time, y = mn)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mn - sd, ymax = mn + sd), width = 0.5) +
    theme_bw() +
    theme(legend.position = "none") +
    ylab(ylabel)
}

#' @export
#' @rdname plot_by
plot_over_time_by_class <- function(summary_df, ylabel, page = 1) {
  ggplot(summary_df, aes(x = time, y = mn, col = class)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mn - sd, ymax = mn + sd), width = 0.5) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ylab(ylabel)
}

#' @param type character, specifying one of "box" or "violin"
#'
#' @export
#' @rdname plot_by
plot_by_class <- function(summary_df, type = c("box", "violin"), page = 1) {
  ggplot(summary_df, aes(x = class, y = mn)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    switch(type,
           box = geom_boxplot(outlier.colour = "grey4", outlier.shape = 21, outlier.size = 1.0),
           violin = geom_violin(outlier.colour = "grey4")
    ) +
    scale_x_discrete(guide = guide_axis(angle = 90)) +
    theme_bw() +
    theme(strip.text.x = element_text(size = 14)) +
    ylab(summary_df$metric)
}
