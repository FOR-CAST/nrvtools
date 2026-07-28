# .parse_metric_labels() errors informatively on an unparseable label

    Code
      .parse_metric_labels("nonsense")
    Condition
      Error:
      ! cannot parse metric label(s): nonsense (expected '<rep>.<prefix>_year<YYYY>_<polyName>')

