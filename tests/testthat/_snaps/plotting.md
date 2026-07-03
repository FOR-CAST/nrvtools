# plot_nrv_envelope draws a box-and-whisker from the five-number summary

    Code
      plot_nrv_envelope(env[, c("time", "poly", "mean", "min", "max")], type = "boxplot")
    Condition
      Error:
      ! plot_nrv_envelope(type = "boxplot") needs column(s): q25, median, q75 (produce them with summarize_nrv())

