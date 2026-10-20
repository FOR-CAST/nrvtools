# plot_nrv_envelope draws a box-and-whisker from the five-number summary

    Code
      plot_nrv_envelope(env[, c("time", "poly", "mean", "min", "max")], type = "boxplot")
    Condition
      Error:
      ! plot_nrv_envelope(type = "boxplot") needs column(s): q25, median, q75 (produce them with summarize_nrv())

# plot_nrv_envelope errors on a current condition without a value column

    Code
      plot_nrv_envelope(new_envelope(), cc = data.frame(poly = "A"))
    Condition
      Error:
      ! no current-condition value column in `cc`: expected one of mean, value (or set `cc_col`)

# plot_by_class errors on a missing column

    Code
      plot_by_class(new_replicate_values())
    Condition
      Error:
      ! plot_by_class(): `summary_df` lacks column(s) mean

