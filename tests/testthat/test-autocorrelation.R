## AR(1) helpers: one long series, reps x years tables as autocorr_summary() reads them, and reps x
## snapshots matrices whose adjacent snapshots have correlation rho (stationary from the first column)
ar1 <- function(phi, n = 20000, seed = 1) {
  set.seed(seed)
  as.numeric(stats::arima.sim(list(ar = phi), n = n))
}
repTables <- function(f, reps = 5, years = 300, seed = 1) {
  lapply(seq_len(reps), function(r) {
    set.seed(seed * 100 + r)
    data.frame(time = seq_len(years), x = f(years))
  })
}
ar1Mat <- function(reps, len, rho) {
  x <- matrix(0, reps, len)
  x[, 1L] <- stats::rnorm(reps) / sqrt(1 - rho^2)
  for (j in seq_len(len)[-1L]) {
    x[, j] <- rho * x[, j - 1L] + stats::rnorm(reps)
  }
  x
}

## ---- autocorr_time(), autocorr_acf(), autocorr_lag_below() ------------------------------------

test_that("tau is 1 for white noise and (1 + phi) / (1 - phi) for an AR(1) decay, exactly", {
  expect_identical(autocorr_time(c(1, rep(0, 20))), 1)
  expect_equal(autocorr_time(0.5^(0:200)), 3)
  expect_equal(autocorr_time(0.9^(0:2000)), 19)
  ## lags 0-200 are an odd number, padded with a 0; lags 0-201 are not
  expect_equal(autocorr_time(0.5^(0:201)), 3)
  ## the sum stops before the first pair sum that is not positive
  expect_equal(autocorr_time(c(1, 0.5, -0.6, 0.1, 0.4, 0.4)), -1 + 2 * 1.5)
  expect_identical(autocorr_time(c(1, 0.5, NA)), NA_real_)
})

test_that("tau of a simulated AR(1) series with phi = 0.9 is about 19, of white noise about 1", {
  expect_equal(autocorr_time(autocorr_acf(ar1(0.9, n = 2e5), 200)), 19, tolerance = 0.1)
  set.seed(2)
  expect_equal(autocorr_time(autocorr_acf(stats::rnorm(2e5), 200)), 1, tolerance = 0.1)
})

test_that("the first lag with ACF below the level is where it is, NA when it never is", {
  expect_identical(autocorr_lag_below(c(1, 0.5, 0.2, 0.05, 0)), 3L)
  expect_identical(autocorr_lag_below(0.8^(0:30), 0.1), 11L)
  expect_identical(autocorr_lag_below(c(1, 0.5, 0.4)), NA_integer_)
  expect_identical(autocorr_lag_below(c(1, NA, 0)), NA_integer_)
})

test_that("autocorr_acf() is stats::acf() unless given a centre and a scale", {
  set.seed(3)
  x <- as.numeric(stats::arima.sim(list(ar = 0.7), 200))
  expect_equal(autocorr_acf(x, 20), as.numeric(stats::acf(x, lag.max = 20, plot = FALSE)$acf))
  ## around a given centre, with a given sum of squares: the lagged products over that sum
  d <- x - 1
  expect_equal(
    autocorr_acf(x, 5, centre = 1, scale = 2 * sum(d^2))[3],
    sum(d[1:198] * d[3:200]) / (2 * sum(d^2))
  )
})

test_that("autocorr_acf() is NA past the series and for series it cannot estimate", {
  rho <- autocorr_acf(c(1, 3, 2, 5), 6)
  expect_length(rho, 7L)
  expect_true(all(!is.na(rho[1:4])))
  expect_true(all(is.na(rho[5:7])))
  expect_true(all(is.na(autocorr_acf(c(1, 2), 3))))
  expect_true(all(is.na(autocorr_acf(rep(2, 10), 3))))
  expect_true(all(is.na(autocorr_acf(1:10, 3, centre = 0, scale = 0))))
})

## ---- ess_ar1(), ess_acf() --------------------------------------------------------------------

test_that("the large-sample ESS is n (1 - r) / (1 + r), with r clipped to [0, 0.999]", {
  expect_equal(ess_ar1(10, 0.5, method = "large_sample"), 10 / 3)
  expect_equal(ess_ar1(10, -0.3, method = "large_sample"), 10)
  expect_equal(ess_ar1(10, 1.2, method = "large_sample"), 10 * 0.001 / 1.999)
  ## additive over series
  expect_equal(ess_ar1(7, 0.5, n_series = 15, method = "large_sample"), 105 / 3)
})

test_that("the finite ESS is n^2 over the sum of the AR(1) correlation matrix", {
  ## the variance of the mean of n AR(1) values of unit variance is sum(r^|i - j|) / n^2
  exact <- function(n, r) n^2 / sum(r^abs(outer(seq_len(n), seq_len(n), "-")))
  expect_equal(ess_ar1(2, 0.5), 4 / 3)
  expect_equal(ess_ar1(3, 0.5), 18 / 11)
  for (r in c(0.2, 0.6, 0.9)) {
    expect_equal(ess_ar1(7, r), exact(7, r))
  }
  expect_equal(ess_ar1(7, 0.9), 1.2534, tolerance = 1e-4)
  expect_equal(ess_ar1(7, c(0, 0.6, NA), n_series = 15), c(105, 15 * exact(7, 0.6), NA))
  ## between 1 and n per series; 1 or 0 values are worth themselves
  expect_equal(ess_ar1(7, c(-0.5, 0, 0.999, 1)), c(7, 7, exact(7, 0.999), exact(7, 0.999)))
  expect_true(all(ess_ar1(7, seq(0, 1, by = 0.05)) >= 1))
  expect_identical(ess_ar1(1, 0.5), 1)
  expect_identical(ess_ar1(0, 0.5, n_series = 15), 0)
  expect_error(ess_ar1(2.5, 0.5), "whole number")
})

test_that("the finite ESS tends to the large-sample one as n grows, and stays above it", {
  for (r in c(0.3, 0.8)) {
    expect_equal(ess_ar1(1e5, r), ess_ar1(1e5, r, method = "large_sample"), tolerance = 1e-3)
    expect_gt(ess_ar1(7, r), ess_ar1(7, r, method = "large_sample"))
  }
  ## at 7 snapshots the large-sample formula falls below one per replicate once r > 6 / 8
  expect_lt(ess_ar1(7, 0.9, n_series = 15, method = "large_sample"), 15)
  expect_gt(ess_ar1(7, 0.9, n_series = 15), 15)
  expect_equal(ess_ar1(7, 0.75, method = "large_sample"), 1)
})

test_that("the finite ESS matches the variance of simulated means of 7 AR(1) values", {
  set.seed(20)
  for (r in c(0.5, 0.8)) {
    means <- rowMeans(ar1Mat(40000, 7, r))
    ## each value has variance 1 / (1 - r^2); the mean of k independent ones would have that over k
    empirical <- (1 / (1 - r^2)) / stats::var(means)
    expect_equal(empirical, ess_ar1(7, r), tolerance = 0.03)
    expect_lt(ess_ar1(7, r, method = "large_sample"), 0.9 * empirical)
  }
})

test_that("ess_acf() is the finite ESS of any ACF, clipped to [1, n] per series", {
  expect_equal(ess_acf(7, 0.6^(0:6), n_series = 15), ess_ar1(7, 0.6, n_series = 15))
  ## lags beyond n - 1 are not used
  expect_equal(ess_acf(3, c(1, 0.5, 0.25, 0.9)), 18 / 11)
  ## a correlation that is not an AR(1) decay: lag 1 only
  expect_equal(ess_acf(4, c(1, 0.5, 0, 0)), 4 / (1 + 2 * 0.75 * 0.5))
  ## strongly negative or near-1 correlations stay within [1, n]
  expect_identical(ess_acf(4, c(1, -0.9, -0.9, -0.9)), 4)
  expect_identical(ess_acf(4, c(1, 1, 1, 1)), 1)
  expect_identical(ess_acf(3, c(1, 0.5, NA)), NA_real_)
  expect_error(ess_acf(7, 0.5^(0:3)), "at least lag n - 1")
})

## ---- autocorr_snapshots() --------------------------------------------------------------------

test_that("snapshot correlations by hand: a shared trend is correlation for 'acf' only", {
  ## two replicates with the same trend and no departure from it
  trend <- rbind(1:7, 1:7)
  expect_true(is.na(autocorr_snapshots(trend)))
  ## around the common mean 4: sum over t of (t - 4)(t - 3), twice, over twice sum((t - 4)^2)
  expect_equal(autocorr_snapshots(trend, method = "acf"), 16 / 28)
  ## a lasting departure from the shared trend is perfect persistence
  offset <- rbind(1:7, 1:7 + 1)
  expect_equal(autocorr_snapshots(offset, lag = 1:3), c(1, 1, 1))
  ## lag 0 is 1; a lag with no pairs is NA
  set.seed(4)
  x <- matrix(stats::rnorm(21), 3, 7)
  expect_equal(autocorr_snapshots(x, lag = c(0, 7, 9)), c(1, NA, NA))
  expect_error(autocorr_snapshots(x, lag = 0.5), "whole numbers")
  expect_error(autocorr_snapshots(x, lag = -1), "whole numbers")
})

test_that("the ensemble correlation is the correlation of deviations from the snapshot means", {
  set.seed(5)
  x <- ar1Mat(15, 7, 0.6)
  d <- sweep(x, 2, colMeans(x))
  a <- c(d[, 1:5])
  b <- c(d[, 3:7])
  expect_equal(autocorr_snapshots(x, lag = 2), sum(a * b) / sqrt(sum(a^2) * sum(b^2)))
  ## a data.frame gives the same
  expect_equal(autocorr_snapshots(as.data.frame(x), lag = 1:6), autocorr_snapshots(x, lag = 1:6))
})

test_that("a missing value drops its pairs and leaves the snapshot means to the other replicates", {
  set.seed(6)
  x <- ar1Mat(6, 5, 0.5)
  y <- x
  y[2, 3] <- NA
  d <- sweep(y, 2, colMeans(y, na.rm = TRUE))
  a <- c(d[, 1:4])
  b <- c(d[, 2:5])
  ok <- !is.na(a) & !is.na(b)
  expect_equal(autocorr_snapshots(y), sum(a[ok] * b[ok]) / sqrt(sum(a[ok]^2) * sum(b[ok]^2)))
  expect_true(is.finite(autocorr_snapshots(y, method = "acf")))
})

test_that("the 'acf' snapshot correlation is autocorr_summary()'s pooled r", {
  set.seed(7)
  x <- ar1Mat(15, 7, 0.6)
  tables <- lapply(seq_len(nrow(x)), function(i) data.frame(time = 1:7, s = x[i, ]))
  res <- autocorr_summary(tables, c(1, 7), max_lag = 1, interval = 1)
  expect_equal(res$fit$r, autocorr_snapshots(x, method = "acf"))
})

## 15 replicates x 7 snapshots (an NRV of 300 years sampled every 50): the bias of each estimator
## over 2000 simulated ensembles with fixed seeds. Expected means from a 4000-ensemble calibration: ensemble
## 0.98-0.99 rho, acf 0.82-0.84 rho, per-replicate Pearson about 0.2 at rho = 0.5.
test_that("on 15 x 7 snapshots the ensemble correlation is nearly unbiased and the ACF is low", {
  pearson <- function(x) {
    d <- x - rowMeans(x)
    stats::cor(c(d[, -ncol(d)]), c(d[, -1L]))
  }
  for (rho in c(0.5, 0.8)) {
    set.seed(round(rho * 100))
    est <- replicate(2000, {
      x <- ar1Mat(15, 7, rho)
      c(
        ensemble = autocorr_snapshots(x),
        acf = autocorr_snapshots(x, method = "acf"),
        pearson = pearson(x)
      )
    })
    bias <- rowMeans(est) / rho
    expect_gt(bias[["ensemble"]], 0.96)
    expect_lt(bias[["ensemble"]], 1.01)
    expect_gt(bias[["acf"]], 0.79)
    expect_lt(bias[["acf"]], 0.87)
    expect_lt(bias[["acf"]], bias[["ensemble"]] - 0.1)
    expect_lt(bias[["pearson"]], 0.6)
  }
})

test_that("a trend shared by the replicates leaves the ensemble correlation unchanged", {
  set.seed(8)
  x <- ar1Mat(15, 7, 0.2)
  trend <- matrix(0:6, 15, 7, byrow = TRUE)
  expect_equal(autocorr_snapshots(x + trend, lag = 1:6), autocorr_snapshots(x, lag = 1:6))
  ## the ACF counts the trend as persistence
  expect_gt(
    autocorr_snapshots(x + trend, method = "acf"),
    autocorr_snapshots(x, method = "acf") + 0.2
  )
})

## ---- autocorr_summary() -----------------------------------------------------------------------

## 15 reps x 301 NRV years of AR(1) with phi 0.95 (tau 39): with each rep centred on its own mean, the
## estimated tau is about 0.75 of the truth (the sample ACF is biased toward 0 when tau is a sizeable fraction
## of the series); around the replicates' common mean it is about 0.95. 30 fixed seeds averaged.
test_that("15 reps x 301 years of AR(1) with phi 0.95 give a mean tau within 15% of 39", {
  tau <- vapply(
    1:30,
    function(seed) {
      tables <- repTables(
        function(n) as.numeric(stats::arima.sim(list(ar = 0.95), n)),
        reps = 15,
        years = 301,
        seed = seed
      )
      autocorr_summary(tables, c(1, 301), 200, 50)$fit$tau
    },
    numeric(1)
  )
  expect_gt(mean(tau) / 39, 0.85)
  expect_lt(mean(tau) / 39, 1.15)
})

test_that("the ACF is pooled around the replicates' common mean", {
  ## two reps with different levels and no variation inside: around their own means there is nothing to
  ## correlate, around the common mean each is perfectly persistent
  tables <- list(data.frame(time = 1:60, x = 0), data.frame(time = 1:60, x = 1))
  fit <- autocorr_summary(tables, c(1, 60), 20, 10)$fit
  expect_gt(fit$phi, 0.9)
})

test_that("each replicate's ACF is a correlation (1 at lag 0, within [-1, 1]); the pooled one is the pooled estimator", {
  ## replicates of very different variance around one level: the per-rep curves are scaled by their own sum of squares
  set.seed(11)
  sd <- c(1, 5, 0.2)
  tables <- lapply(sd, function(k) {
    data.frame(time = 1:200, x = k * as.numeric(stats::arima.sim(list(ar = 0.8), 200)))
  })
  res <- autocorr_summary(tables, c(1, 200), 30, 10)
  perRep <- res$acf[res$acf$rep != "pooled", ]
  expect_equal(perRep$acf[perRep$lag == 0], rep(1, 3))
  expect_true(all(abs(perRep$acf) <= 1 + 1e-12))
  ## the pooled curve: summed lagged products over the summed squares, around the common mean
  x <- lapply(tables, `[[`, "x")
  m <- mean(unlist(x))
  pooledAt <- function(k) {
    sum(vapply(x, function(v) sum((v[seq_len(200 - k)] - m) * (v[seq_len(200 - k) + k] - m)), 0)) /
      sum(vapply(x, function(v) sum((v - m)^2), 0))
  }
  pooled <- res$acf[res$acf$rep == "pooled", ]
  expect_equal(pooled$acf[pooled$lag %in% 0:5], vapply(0:5, pooledAt, 0))
  expect_lte(res$fit$rMax, 1)
  expect_gte(res$fit$rMin, -1)
})

test_that("a series with fewer than 3 values in a replicate is 'tooFew', not 'constant'", {
  set.seed(12)
  tables <- list(
    data.frame(time = 1:100, a = stats::rnorm(100), b = 1, c = stats::rnorm(100)),
    data.frame(time = 1:100, a = c(1, 2, rep(NA, 98)), b = 1, c = stats::rnorm(100))
  )
  res <- autocorr_summary(tables, c(1, 100), 20, 10)
  expect_identical(res$tooFew, "a")
  expect_identical(res$constant, "b")
  expect_setequal(res$undefined, c("a", "b"))
  expect_false(is.na(res$limiting))
})

test_that("a series missing from a replicate's table is left out as 'absent'", {
  set.seed(13)
  tables <- list(
    data.frame(time = 1:50, a = stats::rnorm(50), b = stats::rnorm(50)),
    data.frame(time = 1:50, a = stats::rnorm(50))
  )
  res <- autocorr_summary(tables, c(1, 50), 10, 5)
  expect_identical(res$absent, "b")
  expect_identical(res$fit$series, "a")
})

test_that("r at the summary interval is close to phi^interval and its reps bracket it", {
  tables <- repTables(
    function(n) as.numeric(stats::arima.sim(list(ar = 0.95), n)),
    reps = 20,
    years = 2000
  )
  at100 <- autocorr_summary(tables, c(1, 2000), 200, 100)$fit
  expect_lt(abs(at100$r - 0.95^100), 0.05)
  at20 <- autocorr_summary(tables, c(1, 2000), 200, 20)$fit
  expect_equal(at20$r, 0.95^20, tolerance = 0.1)
  expect_lte(at20$rMin, at20$r)
  expect_gte(at20$rMax, at20$r)
})

test_that("the misfit is near 0 for AR(1) and large for an oscillating AR(2) series", {
  ar1Fit <- autocorr_summary(
    repTables(function(n) as.numeric(stats::arima.sim(list(ar = 0.9), n)), years = 2000),
    c(1, 2000),
    100,
    100
  )$fit
  osc <- autocorr_summary(
    repTables(function(n) as.numeric(stats::arima.sim(list(ar = c(1.4, -0.8)), n)), years = 2000),
    c(1, 2000),
    100,
    100
  )$fit
  expect_lt(ar1Fit$ar1Misfit, 0.1)
  expect_gt(osc$ar1Misfit, 0.3)
})

test_that("the recommended thin is the ceiling of the largest tau; the Geyer tau is kept; a constant series is NA", {
  n <- 600
  tables <- lapply(1:3, function(r) {
    set.seed(r)
    data.frame(time = seq_len(n), slow = ar1(0.9, n, r), fast = ar1(0.3, n, 10 + r), constant = 1)
  })
  res <- autocorr_summary(tables, c(1, n), max_lag = 100, interval = 100)
  fit <- res$fit
  expect_identical(res$thin, ceiling(max(fit$tau, na.rm = TRUE)))
  expect_identical(res$limiting, "slow")
  expect_true(is.na(fit$tau[fit$series == "constant"]))
  expect_true(all(
    c(
      "phi",
      "tau",
      "ar1Misfit",
      "r",
      "rMin",
      "rMax",
      "effectiveSnapshots",
      "effectiveSnapshotsFinite",
      "tauGeyer",
      "lagBelow0.1"
    ) %in%
      names(fit)
  ))
  ## snapshots at 1, 101, ..., 501
  expect_equal(res$nSnapshots, 3 * 6)
  expect_setequal(unique(res$acf$rep), c("1", "2", "3", "pooled"))
})

test_that("the effective snapshots use both ESS formulas over the replicates' snapshots", {
  tables <- repTables(
    function(n) as.numeric(stats::arima.sim(list(ar = 0.98), n)),
    reps = 4,
    years = 301
  )
  res <- autocorr_summary(tables, c(1, 301), 100, 50)
  fit <- res$fit
  expect_equal(res$nSnapshots, 4 * 7)
  expect_equal(fit$effectiveSnapshots, ess_ar1(7, fit$r, n_series = 4, method = "large_sample"))
  expect_equal(fit$effectiveSnapshotsFinite, ess_ar1(7, fit$r, n_series = 4))
  expect_gte(fit$effectiveSnapshotsFinite, fit$effectiveSnapshots)
})

test_that("the snapshot count is exact: on the caller's grid, and with no floating-point shortfall", {
  tables <- repTables(
    function(n) as.numeric(stats::arima.sim(list(ar = 0.8), n)),
    reps = 3,
    years = 1000
  )
  ## the caller's snapshots: an NRV window of 905-1000 on a 700, 750, ... grid holds 950 and 1000
  res <- autocorr_summary(tables, c(905, 1000), 50, 50, snapshot_times = c(950, 1000))
  expect_equal(res$nSnapshots, 3 * 2)
  ## 880-990 on that grid holds 900 and 950, where floor(110 / 50) + 1, counted from 880, is 3
  res <- autocorr_summary(tables, c(880, 990), 50, 50, snapshot_times = c(900, 950))
  expect_equal(res$nSnapshots, 3 * 2)
  ## the default grid starts at the period's start; seq() has no floating-point shortfall
  expect_equal(autocorr_summary(tables, c(1, 301), 50, 50)$nSnapshots, 3 * 7)
  expect_length(seq(0, 0.3, by = 0.1), 4L)
  expect_identical(floor(diff(c(0, 0.3)) / 0.1) + 1, 3)
  expect_error(
    autocorr_summary(tables, c(905, 1000), 50, 50, snapshot_times = c(900, 950)),
    "within `period`"
  )
})

test_that("tau is finite when the series is shorter than the maximum lag", {
  ## the NRV years of a 0.3 NRV window can be fewer than the maximum lag (200)
  tables <- repTables(
    function(n) as.numeric(stats::arima.sim(list(ar = 0.8), n)),
    reps = 4,
    years = 60
  )
  res <- autocorr_summary(tables, c(1, 60), max_lag = 200, interval = 50)
  fit <- res$fit
  expect_true(all(is.finite(c(
    fit$phi,
    fit$tau,
    fit$tauGeyer,
    fit$ar1Misfit,
    fit$r,
    fit$effectiveSnapshots,
    fit$effectiveSnapshotsFinite
  ))))
  expect_true(is.finite(res$thin))
  ## no lag beyond the series' length is made up
  expect_lte(max(res$acf$lag), 59L)
  ## an interval longer than the series has no correlation to report
  long <- autocorr_summary(tables, c(1, 60), max_lag = 200, interval = 100)$fit
  expect_true(is.na(long$r))
  expect_true(is.na(long$effectiveSnapshotsFinite))
  expect_true(is.finite(long$tau))
})
