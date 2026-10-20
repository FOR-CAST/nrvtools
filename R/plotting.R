utils::globalVariables(c(
  "class", "time", "count", "PANEL"
))

## Combine the faceting columns of `df` into a single " | "-separated panel label, as an ORDERED
## factor. Panels follow the ordering of the faceting columns themselves -- a factor column keeps its
## own level order (so e.g. `factor(class, levels = seral_stages())` panels early -> mid -> mature ->
## old instead of ggplot2's default alphabetical "early, mature, mid, old"), and any other column
## sorts naturally. Only observed combinations become levels, so no empty panels are drawn.
.panel_factor <- function(df, facet, sep = " | ") {
  lab <- do.call(paste, c(df[facet], sep = sep))
  keys <- lapply(facet, function(cc) {
    v <- df[[cc]]
    if (is.factor(v)) v else factor(v, levels = sort(unique(v)))
  })
  ord <- do.call(order, keys) ## first facet column is the primary key; the last varies fastest
  factor(lab, levels = unique(lab[ord]))
}

## The columns that identify a reporting unit and metric in the long schema (cf. summarize_nrv()):
## a current-condition value must agree with the plotted data on every one of these that both carry.
.nrv_unit_cols <- c(
  "studyArea",
  "scenario",
  "rptPoly",
  "poly",
  "class",
  "level",
  "metric",
  "metric.1"
)

## First of `candidates` that is a column of `cc`, unless the caller named one (`cc_col`).
.cc_value_col <- function(cc, cc_col, candidates) {
  if (!is.null(cc_col)) {
    return(cc_col)
  }
  hit <- intersect(candidates, names(cc))
  if (!length(hit)) {
    stop(
      "no current-condition value column in `cc`: expected one of ",
      paste(candidates, collapse = ", "),
      " (or set `cc_col`)",
      call. = FALSE
    )
  }
  hit[[1L]]
}

## Current-condition reference values for the panels of `df` (which carries its `.panel` column),
## one per panel and, when `by` is given, per value of that column (e.g. the class on a by-class
## plot). `cc` rows are matched to `df` on `keys` -- the unit columns both tables carry -- rather
## than only on the faceting columns that vary, so a current-condition table spanning other
## subregions or metrics contributes just the rows for the units `df` plots. A `cc` row matching no
## plotted unit is dropped, and when `cc` shares none of `keys` its value applies to every panel.
## Several rows for one panel (and `by` value) are averaged; NA values are dropped. Returns `NULL`
## when nothing is left to draw, else a data.frame of `.panel`, `by` and the value as `.cc`.
.cc_by_panel <- function(cc, df, keys, cc_col, by = NULL) {
  cc <- as.data.frame(cc)
  absent <- setdiff(c(cc_col, by), names(cc))
  if (length(absent)) {
    stop(
      "`cc` lacks column(s) ",
      paste(absent, collapse = ", "),
      " (have: ",
      paste(names(cc), collapse = ", "),
      ")",
      call. = FALSE
    )
  }
  on <- setdiff(intersect(keys, intersect(names(cc), names(df))), by)
  as_chr <- function(d) {
    d[on] <- lapply(d[on], as.character)
    d
  }
  panels <- as_chr(unique(df[c(on, ".panel")]))
  m <- merge(as_chr(cc[c(on, by, cc_col)]), panels, by = on)
  m <- m[!is.na(m[[cc_col]]), , drop = FALSE]
  if (!nrow(m)) {
    return(NULL)
  }
  grp <- c(".panel", by)
  out <- stats::aggregate(m[cc_col], by = m[grp], FUN = mean)
  names(out)[names(out) == cc_col] <- ".cc"
  out[[".panel"]] <- if (is.factor(df[[".panel"]])) {
    factor(as.character(out[[".panel"]]), levels = levels(df[[".panel"]]))
  } else {
    as.character(out[[".panel"]])
  }
  out
}

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

#' Distribution of a metric by class, one panel per reporting unit
#'
#' Draws the distribution of a metric for each class (e.g. leading vegetation
#' type or seral stage) as a box-and-whisker (`type = "box"`) or a violin
#' (`type = "violin"`), with one panel per reporting unit, paginated across
#' figures. Pass the per-replicate values from [nrv_replicate_values()] (with
#' `value_col = "value"`) to show the range of variation across replicates and
#' summary years. The default, `value_col = "mean"`, reads a [summarize_nrv()]
#' envelope instead, so each box spans the across-replicate means of the summary
#' years.
#'
#' Current-condition values passed as `cc` are drawn as red points, one per
#' class and panel, so each class's current value can be read against its
#' simulated range. A class that has a current-condition value but no simulated
#' value in a panel keeps its point, which then stands without a box: the
#' simulation never produced that class there.
#'
#' @param summary_df A `data.frame` with a class column (`class_col`), a value
#'   column (`value_col`) and the faceting column(s). Rows whose value is not
#'   finite -- an undefined metric, such as the standard deviation of a single
#'   patch -- are dropped. `NULL`, or no finite value, returns `NULL`.
#' @param type Plot style: `"box"` (box-and-whisker) or `"violin"`.
#' @param page Page to render (default `1`) via
#'   [ggforce::facet_wrap_paginate()]; use [ggforce::n_pages()] on the `page = 1`
#'   result to get the page count. `NULL` draws every panel in one figure via
#'   [ggplot2::facet_wrap()].
#' @param cc Optional current-condition values: a `data.frame` with `class_col`,
#'   a value column (see `cc_col`) and the unit columns (`poly`, `metric`, ...)
#'   that match its rows to the panels, e.g. [nrv_replicate_values()] of the
#'   current-condition snapshot. A row for a unit that is not plotted is
#'   dropped. `NULL` (default) draws none.
#' @param value_col Name of the value column of `summary_df` (default `"mean"`).
#' @param class_col Name of the class column, on the x axis (default `"class"`).
#' @param facet Faceting column(s); those present are combined into the panel
#'   label (default `"poly"`, one panel per subregion).
#' @param ncol,nrow Panel grid per page (default `4` x `3`).
#' @param scales Facet scales: `"fixed"` (default) shares the y axis across
#'   panels; `"free_y"` scales each panel to its own values.
#' @param xlab,ylab Axis labels. `NULL` (default) uses `class_col`, and the
#'   `metric` name(s) in `summary_df` (else `value_col`).
#' @param title Optional plot title.
#' @param cc_col Column of `cc` holding the current-condition value. `NULL`
#'   (default) uses `value_col` if `cc` has it, else `"mean"`, else `"value"`.
#' @param cc_label Legend label for the current-condition points.
#'
#' @return A `ggplot` object, or `NULL` for empty input.
#'
#' @importFrom rlang .data
#' @export
#' @seealso [nrv_replicate_values()], [plot_nrv_envelope()],
#'   [plot_leading_boxplot()]
plot_by_class <- function(
  summary_df,
  type = c("box", "violin"),
  page = 1,
  cc = NULL,
  value_col = "mean",
  class_col = "class",
  facet = "poly",
  ncol = 4,
  nrow = 3,
  scales = "fixed",
  xlab = NULL,
  ylab = NULL,
  title = NULL,
  cc_col = NULL,
  cc_label = "current condition"
) {
  type <- match.arg(type)
  if (is.null(summary_df) || !nrow(summary_df)) {
    return(invisible(NULL))
  }
  summary_df <- as.data.frame(summary_df)
  absent <- setdiff(c(class_col, value_col), names(summary_df))
  if (length(absent)) {
    stop(
      "plot_by_class(): `summary_df` lacks column(s) ",
      paste(absent, collapse = ", "),
      call. = FALSE
    )
  }
  summary_df <- summary_df[is.finite(summary_df[[value_col]]), , drop = FALSE]
  if (!nrow(summary_df)) {
    return(invisible(NULL))
  }
  facet <- intersect(facet, names(summary_df))
  summary_df[[".panel"]] <- if (length(facet)) .panel_factor(summary_df, facet) else "all"

  ccdf <- NULL
  if (!is.null(cc) && NROW(cc)) {
    ccdf <- .cc_by_panel(
      cc,
      summary_df,
      keys = union(.nrv_unit_cols, facet),
      cc_col = .cc_value_col(cc, cc_col, c(value_col, "mean", "value")),
      by = class_col
    )
  }

  ## one set of class levels for both layers: the data's own order (a factor keeps its levels,
  ## anything else sorts), then any class that only the current condition has.
  cls <- summary_df[[class_col]]
  lev <- if (is.factor(cls)) levels(cls) else sort(unique(as.character(cls)))
  if (!is.null(ccdf)) {
    lev <- c(lev, setdiff(unique(as.character(ccdf[[class_col]])), lev))
    ccdf[[class_col]] <- factor(as.character(ccdf[[class_col]]), levels = lev)
  }
  summary_df[[class_col]] <- factor(as.character(cls), levels = lev)

  gg <- ggplot2::ggplot(summary_df, ggplot2::aes(x = .data[[class_col]], y = .data[[value_col]])) +
    switch(
      type,
      box = ggplot2::geom_boxplot(outlier.colour = "grey4", outlier.shape = 21, outlier.size = 1.0),
      violin = ggplot2::geom_violin()
    )

  if (!is.null(ccdf)) {
    ccdf[[".cc_label"]] <- cc_label
    gg <- gg +
      ggplot2::geom_point(
        data = ccdf,
        ggplot2::aes(x = .data[[class_col]], y = .data[[".cc"]], colour = .data[[".cc_label"]]),
        size = 3,
        inherit.aes = FALSE
      ) +
      ggplot2::scale_colour_manual(values = stats::setNames("red", cc_label), name = NULL)
  }

  facet_layer <- if (!length(facet)) {
    NULL
  } else if (is.null(page)) {
    ggplot2::facet_wrap(stats::as.formula("~ .panel"), ncol = ncol, scales = scales)
  } else {
    ggforce::facet_wrap_paginate(
      stats::as.formula("~ .panel"),
      ncol = ncol,
      nrow = nrow,
      page = page,
      scales = scales
    )
  }
  if (is.null(xlab)) {
    xlab <- class_col
  }
  if (is.null(ylab)) {
    ylab <- if ("metric" %in% names(summary_df)) {
      paste(unique(summary_df[["metric"]]), collapse = ", ")
    } else {
      value_col
    }
  }

  gg <- gg +
    facet_layer +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90)) +
    ggplot2::labs(x = xlab, y = ylab, title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(strip.text.x = ggplot2::element_text(size = 14))
  if (!is.null(ccdf)) {
    gg <- gg + ggplot2::theme(legend.position = "bottom")
  }
  gg
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
#' Current-condition values passed as `cc` are drawn in each panel as a red
#' dashed horizontal line, so the simulated range can be read against today's
#' value; a legend labels the line.
#'
#' @param nrv_df A range-of-variation envelope `data.frame` from
#'   [summarize_nrv()]. `type = "ribbon"` needs `time`, `mean`, `min`, `max`;
#'   `type = "boxplot"` needs `time`, `min`, `q25`, `median`, `q75`, `max`. `NULL`
#'   or a zero-row input returns `NULL`.
#' @param type Plot style: `"ribbon"` (mean line + min--max ribbon) or
#'   `"boxplot"` (box-and-whisker showing median and quartiles).
#' @param facet Candidate faceting columns; those present and varying are combined
#'   into the panel label (default `c("rptPoly", "poly", "class", "metric",
#'   "metric.1")`; `rptPoly` is the reporting polygon LAYER, `poly` the subregion
#'   within it).
#' @param ylab Y-axis label.
#' @param title Optional plot title (e.g. the study area and metric name).
#' @param ncol,nrow Panel grid per page when paginating (default `4` x `3`).
#' @param page Page to render. `NULL` (default) draws every panel in one figure
#'   via [ggplot2::facet_wrap()]; an integer paginates the panels across pages via
#'   [ggforce::facet_wrap_paginate()] so a large panel set is split into multiple
#'   figures. Use [ggforce::n_pages()] on the `page = 1` result to get the page
#'   count, then loop `page` to write one file per page.
#' @param cc Optional current-condition values: typically the [summarize_nrv()]
#'   envelope of the current-condition snapshot (a single replicate, so its
#'   `mean` and `median` both equal the value), or a raw table with a `value`
#'   column. Rows are matched to panels on the unit columns (`poly`, `class`,
#'   `metric`, ...) that `cc` and `nrv_df` share, so `cc` may span more units
#'   than `nrv_df` plots. A row for a unit that `nrv_df` does not plot is
#'   dropped, so no panel is added for it, and a panel with no matching row draws
#'   no line. Several rows for one panel are averaged. `NULL` (default) draws
#'   none.
#' @param cc_col Column of `cc` holding the current-condition value. `NULL`
#'   (default) compares like with like: `"mean"` (the ribbon's line) for
#'   `type = "ribbon"`, `"median"` (the box's middle line) for
#'   `type = "boxplot"`, falling back to `"value"`.
#' @param cc_label Legend label for the current-condition line.
#'
#' @return A `ggplot` object, or `NULL` for empty input.
#'
#' @importFrom rlang .data
#' @export
#' @seealso [plot_by_class()] for the per-class distributions.
plot_nrv_envelope <- function(
  nrv_df,
  type = c("ribbon", "boxplot"),
  facet = c("rptPoly", "poly", "class", "metric", "metric.1"),
  ylab = "value",
  title = NULL,
  ncol = 4,
  nrow = 3,
  page = NULL,
  cc = NULL,
  cc_col = NULL,
  cc_label = "current condition"
) {
  type <- match.arg(type)
  if (is.null(nrv_df) || !nrow(nrv_df)) {
    return(invisible(NULL))
  }
  facet <- intersect(facet, names(nrv_df))
  facet <- facet[vapply(facet, function(cc) length(unique(nrv_df[[cc]])) > 1L, logical(1))]
  nrv_df[[".panel"]] <- if (length(facet)) {
    .panel_factor(nrv_df, facet)
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

  ccPanel <- NULL
  if (!is.null(cc) && NROW(cc)) {
    ccPanel <- .cc_by_panel(
      cc,
      nrv_df,
      keys = union(.nrv_unit_cols, facet),
      cc_col = .cc_value_col(cc, cc_col, c(if (type == "boxplot") "median" else "mean", "value"))
    )
  }
  if (!is.null(ccPanel)) {
    ccPanel[[".cc_label"]] <- cc_label
    gg <- gg +
      ggplot2::geom_hline(
        data = ccPanel,
        ggplot2::aes(
          yintercept = .data[[".cc"]],
          colour = .data[[".cc_label"]],
          linetype = .data[[".cc_label"]]
        ),
        linewidth = 0.8
      ) +
      ggplot2::scale_colour_manual(values = stats::setNames("red", cc_label), name = NULL) +
      ggplot2::scale_linetype_manual(values = stats::setNames("dashed", cc_label), name = NULL)
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

  gg <- gg + facet_layer + ggplot2::labs(x = "time", y = ylab, title = title) + ggplot2::theme_bw()
  if (!is.null(ccPanel)) {
    gg <- gg + ggplot2::theme(legend.position = "bottom")
  }
  gg
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
#'   panel label (default `c("rptPoly", "poly", "class", "metric", "metric.1")`).
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
  facet = c("rptPoly", "poly", "class", "metric", "metric.1"),
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
  df[[".panel"]] <- if (length(facetV)) .panel_factor(df, facetV) else "all"
  ## the current-condition panel labels must live in the SAME factor levels as the histogram data,
  ## otherwise the reference lines land in the wrong (or an extra) panel.
  panel <- function(d) {
    if (!length(facetV)) {
      return("all")
    }
    factor(as.character(.panel_factor(d, facetV)), levels = levels(df[[".panel"]]))
  }

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
