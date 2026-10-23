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
## NA values are dropped. Two cases warn, as they usually mean `cc` is not labelled like `df`: no
## row matches any panel (nothing is drawn), and several rows fall in one panel (and `by` value),
## which are then averaged. Returns `NULL` when nothing is left to draw, else a data.frame of
## `.panel`, `by` and the value as `.cc`.
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
  if (!nrow(m)) {
    warning(
      "no current-condition row matches a plotted unit",
      if (length(on)) paste0(" on ", paste(on, collapse = ", ")),
      "; none is drawn. Check that `cc` labels its units as the plotted data does.",
      call. = FALSE
    )
    return(NULL)
  }
  m <- m[!is.na(m[[cc_col]]), , drop = FALSE]
  if (!nrow(m)) {
    return(NULL)
  }
  grp <- c(".panel", by)
  n <- stats::aggregate(m[cc_col], by = m[grp], FUN = length)[[cc_col]]
  if (any(n > 1L)) {
    warning(
      sum(n > 1L),
      " current-condition value(s) are each the mean of several `cc` rows in one panel",
      if (length(by)) paste0(" and ", by),
      ". Give `cc` the columns that tell those rows apart, or one row per panel.",
      call. = FALSE
    )
  }
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
#' across-replicate proportion of the type's forest (the pixels it leads) in each
#' age class, which sums to 1 across the age classes, with the current-condition
#' proportion marked as a red circle; the circle is hollow, so simulated values
#' at the same place stay visible. For `"All species"` it is the proportion of
#' the forest in each age class. Built with \pkg{ggplot2} (the v2 original used
#' base graphics).
#'
#' @param df A `data.frame` of the raw per-replicate values for one unit, with an
#'   age-class column (`class_col`) and a proportion column (`value_col`); one row
#'   per replicate x summary-year x age class.
#' @param cc Optional `data.frame` of current-condition values (same columns as
#'   `df`), one value per age class, drawn as red circles. `NULL` omits them.
#' @param ageClasses Age-class labels in order young -> old; the y-axis is drawn
#'   top (`Young`) to bottom (`Old`). Default `c("Young","Immature","Mature","Old")`.
#' @param value_col,class_col Column names for the proportion and age class.
#' @param species_col Column naming the vegetation type, used for the default
#'   `xlab`.
#' @param xlab Axis label. `NULL` (default) names the type in `species_col`:
#'   "Proportion of `<type>`-leading forest area", or "Proportion of forest area"
#'   for `"All species"`.
#' @param title Plot title.
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
  species_col = "metric.1",
  xlab = NULL,
  title = NULL,
  caption = NULL
) {
  if (is.null(df) || !nrow(df)) {
    return(invisible(NULL))
  }
  if (is.null(xlab)) {
    xlab <- .leading_xlab(unique(df[[species_col]]))
  }
  df[[class_col]] <- factor(df[[class_col]], levels = rev(ageClasses))
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]], y = .data[[class_col]])) +
    ggplot2::geom_boxplot(fill = "limegreen", outlier.size = 0.8) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(x = xlab, y = "Age class", title = title, caption = caption) +
    ggplot2::theme_bw()

  if (!is.null(cc) && nrow(cc)) {
    cc[[class_col]] <- factor(cc[[class_col]], levels = rev(ageClasses))
    gg <- gg + .cc_circles(cc, value_col, class_col, size = 4)
  }
  gg
}

## x-axis label of a leading-type boxplot: the share of one type's forest in each age class
.leading_xlab <- function(type) {
  if (length(type) != 1L || is.na(type)) {
    "Proportion of the type's forest area"
  } else if (identical(type, "All species")) {
    "Proportion of forest area"
  } else {
    paste0("Proportion of ", gsub("_", " ", type), "-leading forest area")
  }
}

## the current condition as hollow red circles, so a simulated value (box or outlier) at the same
## place stays visible
.cc_circles <- function(cc, value_col, y_col, size) {
  ggplot2::geom_point(
    data = cc,
    ggplot2::aes(x = .data[[value_col]], y = .data[[y_col]]),
    shape = 21,
    colour = "red",
    fill = NA,
    stroke = 1.5,
    size = size
  )
}

#' Leading-vegetation shares of the forest, by species and age class
#'
#' One figure per reporting subregion: a panel per age class (young to old, left
#' to right) and, in each, a horizontal box per leading vegetation class of its
#' across-replicate share of the subregion's forested area (the
#' `"leadingShare"` metric of [leadingVegByAgeClass()]). Every box has the same
#' denominator, so a class that leads on little of the forest has a small share,
#' where its proportions by age class ([plot_leading_boxplot()]) swing between 0
#' and 1. Current-condition shares are drawn as hollow red circles.
#'
#' Classes are ordered by their mean simulated share, the largest at the top. A
#' class that is 0 in every simulated and current-condition row is left out; one
#' with current-condition values only keeps its row and its circles.
#'
#' @param df A `data.frame` of the raw per-replicate values for one subregion,
#'   with age-class (`class_col`), vegetation-class (`species_col`) and share
#'   (`value_col`) columns; one row per replicate x summary-year x age class x
#'   vegetation class. A replicate-year in which a class leads nowhere should
#'   hold a 0 rather than be missing.
#' @param cc Optional `data.frame` of current-condition values (same columns as
#'   `df`), drawn as hollow red circles. `NULL` omits them.
#' @param ageClasses Age-class labels in order young -> old (panel order).
#' @param value_col,class_col,species_col Column names for the share, the age
#'   class and the vegetation class.
#' @param species_labels Optional named character vector of display labels,
#'   named by vegetation-class code. Codes without a label are shown as they are.
#' @param all_label The roll-up class left out of the figure, as it is not a
#'   vegetation class of its own; `NULL` keeps every class.
#' @param xlab,title,caption Axis label, plot title and caption.
#'
#' @return A `ggplot` object (`NULL` for empty input).
#'
#' @importFrom rlang .data
#' @export
#' @seealso [plot_leading_boxplot()], [leadingVegByAgeClass()]
plot_leading_share <- function(
  df,
  cc = NULL,
  ageClasses = c("Young", "Immature", "Mature", "Old"),
  value_col = "value",
  class_col = "class",
  species_col = "metric.1",
  species_labels = NULL,
  all_label = "All species",
  xlab = "Proportion of forested area",
  title = NULL,
  caption = NULL
) {
  if (is.null(df) || !nrow(df)) {
    return(invisible(NULL))
  }
  if (!is.null(cc) && !nrow(cc)) {
    cc <- NULL
  }
  if (!is.null(all_label)) {
    df <- df[df[[species_col]] != all_label, , drop = FALSE]
    if (!is.null(cc)) cc <- cc[cc[[species_col]] != all_label, , drop = FALSE]
  }

  ## classes that lead somewhere, ordered by mean simulated share (the largest drawn at the top)
  leads <- function(d) unique(d[[species_col]][!is.na(d[[value_col]]) & d[[value_col]] != 0])
  keep <- union(leads(df), if (!is.null(cc)) leads(cc))
  if (!length(keep)) {
    return(invisible(NULL))
  }
  mn <- tapply(df[[value_col]], df[[species_col]], mean, na.rm = TRUE)[keep]
  mn[is.na(mn)] <- 0
  lev <- keep[order(mn, method = "radix")]
  lab <- lev
  if (!is.null(species_labels)) {
    hit <- lev %in% names(species_labels)
    lab[hit] <- species_labels[lev[hit]]
  }

  df <- df[df[[species_col]] %in% lev, , drop = FALSE]
  df[[species_col]] <- factor(df[[species_col]], levels = lev)
  df[[class_col]] <- factor(df[[class_col]], levels = ageClasses)
  gg <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[value_col]], y = .data[[species_col]])) +
    ggplot2::geom_boxplot(fill = "limegreen", outlier.size = 0.8, orientation = "y") +
    ggplot2::facet_wrap(ggplot2::vars(.data[[class_col]]), nrow = 1, drop = FALSE) +
    ggplot2::scale_x_continuous(
      breaks = seq(0, 1, 0.25),
      labels = c("0", "0.25", "0.5", "0.75", "1")
    ) +
    ggplot2::scale_y_discrete(labels = stats::setNames(lab, lev), drop = FALSE) +
    ggplot2::coord_cartesian(xlim = c(0, 1)) +
    ggplot2::labs(x = xlab, y = NULL, title = title, caption = caption) +
    ggplot2::theme_bw()

  if (!is.null(cc)) {
    cc <- cc[cc[[species_col]] %in% lev, , drop = FALSE]
    cc[[species_col]] <- factor(cc[[species_col]], levels = lev)
    cc[[class_col]] <- factor(cc[[class_col]], levels = ageClasses)
    gg <- gg + .cc_circles(cc, value_col, species_col, size = 3)
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
#' @param caption Optional caption below the figure. `NULL` (default) omits it.
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
  title = NULL,
  caption = NULL
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
    ggplot2::labs(x = xlab, y = "Proportion in NRV", title = title, caption = caption) +
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
#' @param xlab character, the label for the x-axis (default `"time"`).
#'
#' @param caption Optional caption below the figure. `NULL` (default) omits it.
#'
#' @return ggplot object; invoked for side effect of plotting
#'
#' @export
#' @rdname plot_by
plot_over_time <- function(summary_df, ylabel, page = 1, xlab = "time", caption = NULL) {
  ggplot(summary_df, aes(x = time, y = mean)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_bw() +
    theme(legend.position = "none") +
    ggplot2::labs(x = xlab, y = ylabel, caption = caption)
}

#' @export
#' @rdname plot_by
plot_over_time_by_class <- function(summary_df, ylabel, page = 1, xlab = "time", caption = NULL) {
  ggplot(summary_df, aes(x = time, y = mean, col = class)) +
    ggforce::facet_wrap_paginate(~poly, ncol = 4, nrow = 3, page = page) +
    geom_point() +
    geom_line() +
    geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), width = 0.5) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    theme_bw() +
    theme(legend.position = "bottom") +
    ggplot2::labs(x = xlab, y = ylabel, caption = caption)
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
#' value in a panel keeps its point, which then stands without a box: either the
#' simulation never produced that class there, or the metric is undefined
#' (not finite) in every replicate and year where it did.
#'
#' The panels and the classes on the x axis come from every row of
#' `summary_df`, before rows whose value is not finite are dropped, so a
#' subregion or class whose values are all undefined keeps its empty panel or
#' axis position, and the page count matches the input's subregions.
#'
#' @param summary_df A `data.frame` with a class column (`class_col`), a value
#'   column (`value_col`) and the faceting column(s). Rows whose value is not
#'   finite -- an undefined metric, such as the standard deviation of a single
#'   patch -- are dropped, with a message giving their number. `NULL`, or no
#'   finite value, returns `NULL`.
#' @param type Plot style: `"box"` (box-and-whisker) or `"violin"`.
#' @param page Page to render (default `1`) via
#'   [ggforce::facet_wrap_paginate()]; use [ggforce::n_pages()] on the `page = 1`
#'   result to get the page count. `NULL` draws every panel in one figure via
#'   [ggplot2::facet_wrap()].
#' @param cc Optional current-condition values: a `data.frame` with `class_col`,
#'   a value column (see `cc_col`) and the unit columns (`poly`, `metric`, ...)
#'   that match its rows to the panels, e.g. [nrv_replicate_values()] of the
#'   current-condition snapshot. A row for a unit that is not plotted is
#'   dropped. A warning says when no row matches a panel, or when several rows
#'   fall in one panel and class (their mean is drawn). `NULL` (default) draws
#'   none.
#' @param value_col Name of the value column of `summary_df` (default `"mean"`).
#' @param class_col Name of the class column, on the x axis (default `"class"`).
#' @param facet Faceting column(s); those present are combined into the panel
#'   label (default `"poly"`, one panel per subregion). A single column is
#'   faceted on directly, so a layer added to the result with its own data and
#'   that column lands in the matching panel.
#' @param ncol,nrow Panel grid per page (default `4` x `3`).
#' @param scales Facet scales: `"fixed"` (default) shares the y axis across
#'   panels; `"free_y"` scales each panel to its own values.
#' @param xlab,ylab Axis labels. `NULL` (default) uses `class_col`, and the
#'   `metric` name(s) in `summary_df` (without a `metric` column, no y title).
#' @param title Optional plot title.
#' @param cc_col Column of `cc` holding the current-condition value. `NULL`
#'   (default) uses `value_col` if `cc` has it, else `"mean"`, else `"value"`.
#' @param cc_label Legend label for the current-condition points.
#' @param caption Optional caption below the figure, e.g. what the boxes
#'   summarise. `NULL` (default) omits it.
#'
#' @return A `ggplot` object, or `NULL` for empty input. The y axis labels use
#'   thousands separators.
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
  cc_label = "current condition",
  caption = NULL
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
  ## Panels, classes and the y title come from every row, as in 0.3.4, where ggplot2 dropped the
  ## rows that are not finite only when drawing; the current-condition points match those panels.
  facet <- intersect(facet, names(summary_df))
  summary_df[[".panel"]] <- if (length(facet)) .panel_factor(summary_df, facet) else "all"
  cls <- summary_df[[class_col]]
  lev <- if (is.factor(cls)) levels(droplevels(cls)) else sort(unique(as.character(cls)))
  if (is.null(xlab)) {
    xlab <- class_col
  }
  if (is.null(ylab) && "metric" %in% names(summary_df)) {
    ylab <- paste(unique(summary_df[["metric"]]), collapse = ", ")
  }

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

  ok <- is.finite(summary_df[[value_col]])
  if (!any(ok)) {
    return(invisible(NULL))
  }
  if (!all(ok)) {
    message("plot_by_class(): dropped ", sum(!ok), " row(s) whose `", value_col, "` is not finite")
    summary_df <- summary_df[ok, , drop = FALSE]
  }

  ## one set of class levels for both layers: the data's own order (a factor keeps its levels,
  ## anything else sorts), then any class that only the current condition has.
  if (!is.null(ccdf)) {
    lev <- c(lev, setdiff(unique(as.character(ccdf[[class_col]])), lev))
    ccdf[[class_col]] <- factor(as.character(ccdf[[class_col]]), levels = lev)
  }
  summary_df[[class_col]] <- factor(as.character(summary_df[[class_col]]), levels = lev)

  ## a single faceting column is faceted on directly, as in 0.3.4, so that a layer added with its own
  ## data lands in its own panel instead of in every one; several share the combined `.panel` label
  fcol <- if (length(facet) == 1L) facet else ".panel"
  if (length(facet) == 1L) {
    summary_df[[fcol]] <- summary_df[[".panel"]]
    if (!is.null(ccdf)) {
      ccdf[[fcol]] <- ccdf[[".panel"]]
    }
  }

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

  ## drop = FALSE keeps the panels of subregions whose rows were all dropped above
  facet_layer <- if (!length(facet)) {
    NULL
  } else if (is.null(page)) {
    ggplot2::facet_wrap(
      ggplot2::vars(!!rlang::sym(fcol)),
      ncol = ncol,
      scales = scales,
      drop = FALSE
    )
  } else {
    ggforce::facet_wrap_paginate(
      ggplot2::vars(!!rlang::sym(fcol)),
      ncol = ncol,
      nrow = nrow,
      page = page,
      scales = scales,
      drop = FALSE
    )
  }

  gg <- gg +
    facet_layer +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 90), drop = FALSE) +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = xlab, y = ylab, title = title, caption = caption) +
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
#' Current-condition values passed as `cc` are drawn as a red dashed horizontal
#' line in each panel where `cc` has a value, so the simulated range can be read
#' against today's value; a legend labels the line.
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
#' @param ncol,nrow Panel grid. With `page`, the grid of each page; `NULL`
#'   (default) is `4` x `3`. Without `page`, passed to [ggplot2::facet_wrap()],
#'   where `NULL` lets it choose.
#' @param page Page to render. `NULL` (default) draws every panel in one figure
#'   via [ggplot2::facet_wrap()]; an integer paginates the panels across pages via
#'   [ggforce::facet_wrap_paginate()] so a large panel set is split into multiple
#'   figures. Use [ggforce::n_pages()] on the `page = 1` result to get the page
#'   count, then loop `page` to write one file per page.
#' @param cc Optional current-condition values: typically the [summarize_nrv()]
#'   envelope of the current-condition snapshot, or a table with a `value`
#'   column. A class- or landscape-level metric has one value per unit in the
#'   snapshot, so that envelope's `mean` and `median` both equal it. A
#'   patch-level metric has one value per patch, and the snapshot's patch mean
#'   and median can differ many times over; summarise [nrv_replicate_values()]
#'   of the snapshot instead, to get one value per unit (the mean over its
#'   patches) as for the replicates. Rows are matched to panels on the unit
#'   columns (`poly`, `class`, `metric`, ...) that `cc` and `nrv_df` share, so
#'   `cc` may span more units than `nrv_df` plots. A row for a unit that `nrv_df`
#'   does not plot is dropped, so no panel is added for it, and a panel with no
#'   matching row draws no line. A warning says when no row matches a panel, or
#'   when several rows fall in one panel (their mean is drawn). `NULL` (default)
#'   draws none.
#' @param cc_col Column of `cc` holding the current-condition value. `NULL`
#'   (default) compares like with like: `"mean"` (the ribbon's line) for
#'   `type = "ribbon"`, falling back to `"value"`; `"median"` (the box's middle
#'   line) for `type = "boxplot"`, falling back to `"value"`, then `"mean"`.
#' @param cc_label Legend label for the current-condition line.
#' @param xlab X-axis label (default `"time"`).
#' @param caption Optional caption below the figure, e.g. what the band or
#'   boxes summarise. `NULL` (default) omits it.
#'
#' @return A `ggplot` object, or `NULL` for empty input. The y axis labels use
#'   thousands separators.
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
  ncol = NULL,
  nrow = NULL,
  page = NULL,
  cc = NULL,
  cc_col = NULL,
  cc_label = "current condition",
  xlab = "time",
  caption = NULL
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
      cc_col = .cc_value_col(
        cc,
        cc_col,
        if (type == "boxplot") c("median", "value", "mean") else c("mean", "value")
      )
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
    ggplot2::facet_wrap(stats::as.formula("~ .panel"), ncol = ncol, nrow = nrow, scales = "free_y")
  } else {
    ggforce::facet_wrap_paginate(
      stats::as.formula("~ .panel"),
      ncol = if (is.null(ncol)) 4L else ncol,
      nrow = if (is.null(nrow)) 3L else nrow,
      page = page,
      scales = "free_y"
    )
  }

  gg <- gg +
    facet_layer +
    ggplot2::scale_y_continuous(labels = scales::label_comma()) +
    ggplot2::labs(x = xlab, y = ylab, title = title, caption = caption) +
    ggplot2::theme_bw()
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
#' @param caption Optional caption below the figure. `NULL` (default) omits it.
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
  ylab = "Proportion in NRV",
  caption = NULL
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
    ggplot2::labs(x = xlab, y = ylab, caption = caption) +
    ggplot2::theme_bw()
}
