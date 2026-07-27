utils::globalVariables(c(
  "class", "time", "count", "PANEL"
))

#' Leading-vegetation age-class boxplot (v2 LandWeb_summary form)
#'
#' Reproduces the v2 "Leading vegetation by age class" boxplot for a single
#' reporting unit (subregion x species): horizontal box-and-whiskers of the
#' across-replicate proportion of forest area in each age class, with the
#' current-condition proportion marked as a red dot. Built with \pkg{ggplot2}
#' (the v2 original used base graphics).
#'
#' @param df A `data.frame` of the raw per-replicate values for one unit, with an
#'   age-class column (`class_col`) and a proportion column (`value_col`); one row
#'   per replicate x summary-year x age class.
#' @param cc Optional `data.frame` of current-condition values (same columns as
#'   `df`), one value per age class, drawn as red dots. `NULL` omits them.
#' @param ageClasses Age-class labels in order young -> old; the y-axis is drawn
#'   bottom (`Young`) to top (`Old`). Default `c("Young","Immature","Mature","Old")`.
#' @param value_col,class_col Column names for the proportion and age class.
#' @param xlab,title Axis label and plot title.
#' @param caption Optional caption, rendered bottom-right below the figure (e.g.
#'   the subregion's total forested area). `NULL` omits it.
#'
#' @return A `ggplot` object (`NULL` for empty input).
#'
#' @importFrom rlang .data
#' @export
#' @seealso [plot_largepatch_histogram()], [leadingVegByAgeClass()]
plot_leading_boxplot <- function(
  df,
  cc = NULL,
  ageClasses = c("Young", "Immature", "Mature", "Old"),
  value_col = "value",
  class_col = "class",
  xlab = "Proportion of forest area",
  title = NULL,
  caption = NULL
) {
  if (is.null(df) || !nrow(df)) {
    return(invisible(NULL))
  }
  df[[class_col]] <- factor(df[[class_col]], levels = rev(ageClasses))
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]], y = .data[[class_col]])) +
    ggplot2::geom_boxplot(fill = "limegreen", outlier.size = 0.8) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(x = xlab, y = "Age class", title = title, caption = caption) +
    ggplot2::theme_bw()

  if (!is.null(cc) && nrow(cc)) {
    cc[[class_col]] <- factor(cc[[class_col]], levels = rev(ageClasses))
    gg <- gg +
      ggplot2::geom_point(
        data = cc,
        ggplot2::aes(x = .data[[value_col]], y = .data[[class_col]]),
        colour = "red",
        size = 4
      )
  }
  gg
}

#' Large-patch age-class histogram (v2 LandWeb_summary form, one file per species)
#'
#' Reproduces the v2 "LargePatches" histogram for a single reporting unit
#' (subregion x species x size threshold), but grouped one file per species with
#' **four age-class panels** (young -> old) instead of one file per age class.
#' Each panel is the across-replicate distribution ("Proportion in NRV") of the
#' number of patches at or above the size threshold, with the current-condition
#' count marked as a red vertical line.
#'
#' @param df A `data.frame` of the raw per-replicate values for one
#'   species x size unit, with an age-class column (`class_col`) and a count
#'   column (`value_col`); one row per replicate x summary-year x age class.
#' @param cc Optional `data.frame` of current-condition values (same columns),
#'   one per age class, drawn as red vertical lines. `NULL` omits them.
#' @param ageClasses Age-class labels in order young -> old (panel order).
#' @param value_col,class_col Column names for the count and age class.
#' @param bins Number of histogram bins (default `30`).
#' @param xlab,title Axis label and plot title.
#'
#' @return A `ggplot` object (`NULL` for empty input).
#'
#' @importFrom rlang .data
#' @export
#' @seealso [plot_leading_boxplot()], [largePatchCounts()]
plot_largepatch_histogram <- function(
  df,
  cc = NULL,
  ageClasses = c("Young", "Immature", "Mature", "Old"),
  value_col = "value",
  class_col = "class",
  bins = 30,
  xlab = "Number of patches",
  title = NULL
) {
  if (is.null(df) || !nrow(df)) {
    return(invisible(NULL))
  }
  df[[class_col]] <- factor(df[[class_col]], levels = ageClasses)
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]])) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count / tapply(count, PANEL, sum)[PANEL])),
      bins = bins,
      colour = "grey40",
      fill = "grey70"
    ) +
    ggplot2::facet_wrap(ggplot2::vars(.data[[class_col]]), ncol = 2, drop = FALSE) +
    ggplot2::labs(x = xlab, y = "Proportion in NRV", title = title) +
    ggplot2::theme_bw()

  if (!is.null(cc) && nrow(cc)) {
    cc[[class_col]] <- factor(cc[[class_col]], levels = ageClasses)
    gg <- gg +
      ggplot2::geom_vline(
        data = cc,
        ggplot2::aes(xintercept = .data[[value_col]]),
        colour = "red",
        linewidth = 1
      )
  }
  gg
}

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
#' Plots the across-replicate range of variation from an envelope produced by
#' [summarize_nrv()], in one of two styles:
#'
#' * `type = "ribbon"` -- the across-replicate mean line with a min--max ribbon;
#' * `type = "boxplot"` -- a box-and-whisker per time step built from the
#'   five-number summary (`min`, `q25`, `median`, `q75`, `max`) via
#'   `geom_boxplot(stat = "identity")`, so the median and quartiles are shown
#'   (the mean-line/ribbon hides them).
#'
#' Both facet by whichever of the requested categorical columns actually vary, so
#' each distinct combination gets its own panel and replicate envelopes never
#' overlay ambiguously within a panel; a constant column is dropped from the panel
#' label, and trivial data collapses to a single panel.
#'
#' @param nrv_df A range-of-variation envelope `data.frame` from
#'   [summarize_nrv()]. `type = "ribbon"` needs `time`, `mean`, `min`, `max`;
#'   `type = "boxplot"` needs `time`, `min`, `q25`, `median`, `q75`, `max`. `NULL`
#'   or a zero-row input returns `NULL`.
#' @param type Plot style: `"ribbon"` (mean line + min--max ribbon) or
#'   `"boxplot"` (box-and-whisker showing median and quartiles).
#' @param facet Candidate faceting columns; those present and varying are combined
#'   into the panel label (default `c("poly", "class", "metric", "metric.1")`).
#' @param ylab Y-axis label.
#' @param title Optional plot title (e.g. the study area and metric name).
#' @param ncol,nrow Panel grid per page when paginating (default `4` x `3`).
#' @param page Page to render. `NULL` (default) draws every panel in one figure
#'   via [ggplot2::facet_wrap()]; an integer paginates the panels across pages via
#'   [ggforce::facet_wrap_paginate()] so a large panel set is split into multiple
#'   figures. Use [ggforce::n_pages()] on the `page = 1` result to get the page
#'   count, then loop `page` to write one file per page.
#'
#' @return A `ggplot` object, or `NULL` for empty input.
#'
#' @importFrom rlang .data
#' @export
plot_nrv_envelope <- function(
  nrv_df,
  type = c("ribbon", "boxplot"),
  facet = c("poly", "class", "metric", "metric.1"),
  ylab = "value",
  title = NULL,
  ncol = 4,
  nrow = 3,
  page = NULL
) {
  type <- match.arg(type)
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

  if (type == "boxplot") {
    need <- c("min", "q25", "median", "q75", "max")
    miss <- setdiff(need, names(nrv_df))
    if (length(miss)) {
      stop(
        "plot_nrv_envelope(type = \"boxplot\") needs column(s): ",
        paste(miss, collapse = ", "),
        " (produce them with summarize_nrv())",
        call. = FALSE
      )
    }
    ## discrete x so each snapshot time gets one identity box
    gg <- ggplot2::ggplot(nrv_df, ggplot2::aes(x = factor(.data[["time"]]))) +
      ggplot2::geom_boxplot(
        ggplot2::aes(
          ymin = .data[["min"]],
          lower = .data[["q25"]],
          middle = .data[["median"]],
          upper = .data[["q75"]],
          ymax = .data[["max"]]
        ),
        stat = "identity"
      )
  } else {
    gg <- ggplot2::ggplot(nrv_df, ggplot2::aes(x = .data[["time"]])) +
      ggplot2::geom_ribbon(
        ggplot2::aes(ymin = .data[["min"]], ymax = .data[["max"]]),
        alpha = 0.25
      ) +
      ggplot2::geom_line(ggplot2::aes(y = .data[["mean"]]))
  }

  ## no categorical column varies -> a single series: draw one plain panel (no meaningless facet
  ## strip). Otherwise facet by the panel label, paginating when `page` is given.
  facet_layer <- if (!length(facet)) {
    NULL
  } else if (is.null(page)) {
    ggplot2::facet_wrap(stats::as.formula("~ .panel"), scales = "free_y")
  } else {
    ggforce::facet_wrap_paginate(
      stats::as.formula("~ .panel"),
      ncol = ncol,
      nrow = nrow,
      page = page,
      scales = "free_y"
    )
  }

  gg + facet_layer + ggplot2::labs(x = "time", y = ylab, title = title) + ggplot2::theme_bw()
}

## collect a raw long metric table from parquet paths/root, an Arrow dataset/query, or a data.frame.
.collect_nrv_values <- function(x) {
  if (is.data.frame(x)) {
    return(x)
  }
  ds <- if (inherits(x, c("Dataset", "arrow_dplyr_query"))) x else open_nrv_dataset(x)
  if (is.null(ds)) {
    return(NULL)
  }
  as.data.frame(dplyr::collect(ds))
}

#' Plot the across-replicate distribution of a metric, with a reference line
#'
#' The LandWeb-summary counterpart to [plot_nrv_envelope()]: instead of an
#' envelope over `time`, it plots the *distribution across replicates* (pooled over
#' the summary period) of the raw per-replicate values as a histogram, one panel
#' per faceting-column combination, with an optional red vertical reference line at
#' the current-condition value. This reproduces the v2 `LandWeb_summary` histograms
#' ("Proportion in NRV" vs the metric, with the current-condition marker), which
#' are deliberately not time series.
#'
#' Because the histogram needs the raw per-replicate values (not the collapsed
#' envelope), pass the replicate parquet(s) -- as produced by [write_nrv_parquet()]
#' -- rather than a [summarize_nrv()] envelope.
#'
#' @param x Raw per-replicate values: parquet file paths / a dataset root (see
#'   [open_nrv_dataset()]), an Arrow `Dataset`/query, or a `data.frame`.
#' @param cc Optional current-condition values (same forms as `x`); their per-panel
#'   value is drawn as a red vertical reference line. `NULL` (default) omits it.
#' @param value_col Name of the value column to bin (default `"value"`).
#' @param facet Candidate faceting columns; those present and varying become the
#'   panel label (default `c("poly", "class", "metric", "metric.1")`).
#' @param bins Number of histogram bins (default `30`).
#' @param xlab,ylab Axis labels.
#'
#' @return A `ggplot` object, or `NULL` for empty input.
#'
#' @importFrom rlang .data
#' @export
#' @seealso [plot_nrv_envelope()], [calculateLandWebMetrics()]
plot_nrv_distribution <- function(
  x,
  cc = NULL,
  value_col = "value",
  facet = c("poly", "class", "metric", "metric.1"),
  bins = 30,
  xlab = value_col,
  ylab = "Proportion in NRV"
) {
  df <- .collect_nrv_values(x)
  if (is.null(df) || !nrow(df)) {
    return(invisible(NULL))
  }
  facetV <- intersect(facet, names(df))
  facetV <- facetV[vapply(facetV, function(cc) length(unique(df[[cc]])) > 1L, logical(1))]
  panel <- function(d) if (length(facetV)) do.call(paste, c(d[facetV], sep = " | ")) else "all"
  df[[".panel"]] <- panel(df)

  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]])) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(count / tapply(count, PANEL, sum)[PANEL])),
      bins = bins,
      colour = "grey40",
      fill = "grey70"
    )

  if (!is.null(cc)) {
    ccdf <- .collect_nrv_values(cc)
    if (!is.null(ccdf) && nrow(ccdf)) {
      ccdf[[".panel"]] <- panel(ccdf)
      ccPanel <- stats::aggregate(
        stats::as.formula(paste(value_col, "~ .panel")),
        data = ccdf,
        FUN = function(v) mean(v, na.rm = TRUE)
      )
      gg <- gg +
        ggplot2::geom_vline(
          data = ccPanel,
          ggplot2::aes(xintercept = .data[[value_col]]),
          colour = "red",
          linewidth = 1
        )
    }
  }

  gg +
    ggplot2::facet_wrap(stats::as.formula("~ .panel"), scales = "free") +
    ggplot2::labs(x = xlab, y = ylab) +
    ggplot2::theme_bw()
}
