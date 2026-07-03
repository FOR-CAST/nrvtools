# summarize_nrv errors on a missing value column

    Code
      summarize_nrv(root, value_col = "nope")
    Condition
      Error:
      ! summarize_nrv(): value column 'nope' not in dataset (have: time, poly, value, replicate)

