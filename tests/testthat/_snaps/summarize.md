# summarize_nrv errors on a missing value column

    Code
      summarize_nrv(root, value_col = "nope")
    Condition
      Error:
      ! summarize_nrv(): value column 'nope' not in dataset (have: time, poly, value, replicate)

# nrv_replicate_values errors on a missing value column

    Code
      nrv_replicate_values(new_replicate(1), value_col = "nope")
    Condition
      Error:
      ! nrv_replicate_values(): value column 'nope' not in data (have: time, poly, class, level, metric, value)

