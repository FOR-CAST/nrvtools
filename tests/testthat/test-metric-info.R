test_that("nrv_metric_info gives each metric a plain name and unit", {
  i <- nrv_metric_info(
    c("ca", "ed", "sam_mdn", "area_cv", "interior_prop", "nope"),
    c("class", "landscape", "patch", "class", "class", "class")
  )
  expect_equal(
    i$label,
    c(
      "Total class area (ha)",
      "Edge density (m/ha)",
      "Median stand age of a patch (years)",
      "Coefficient of variation of patch area (%)",
      "Interior forest, proportion of class area",
      "nope"
    )
  )
  expect_equal(i$unit, c("ha", "m/ha", "years", "%", NA, NA))
  expect_equal(i$name[[6]], NA_character_)
})

test_that("nrv_metric_info matches the level when given one", {
  expect_equal(nrv_metric_info("area_mn")$level, "class") ## first entry for the code
  expect_equal(nrv_metric_info("area_mn", "landscape")$level, "landscape")
  expect_equal(nrv_metric_info(c("ai", "ai"), c(NA, "landscape"))$level, c("class", "landscape"))
  expect_true(is.na(nrv_metric_info("sam_mdn", "class")$name)) ## not at that level
  expect_equal(nrow(nrv_metric_info(character(0))), 0L)
})

test_that("nrv_metric_info covers every metric of the default lists, once", {
  tab <- nrv_metric_info()
  expect_named(tab, c("level", "metric", "name", "unit", "label"))
  expect_false(anyDuplicated(paste(tab$level, tab$metric)) > 0L)
  fns <- unlist(c(default_landscape_metrics(), default_patch_metrics_seral()))
  fns <- fns[startsWith(fns, "lsm_")]
  level <- c(p = "patch", c = "class", l = "landscape")[substr(fns, 5, 5)]
  expect_true(all(paste(level, substring(fns, 7)) %in% paste(tab$level, tab$metric)))
})
