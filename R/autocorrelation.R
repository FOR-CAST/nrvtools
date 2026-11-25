## Autocorrelation of replicated landscape series, and the number of independent values it leaves.
## NRV snapshots are taken some years apart so that they are close to independent samples, as thinning
## does for MCMC; these functions measure how close.

#' Sample autocorrelation function of one series
#'
#' The correlation of a series with itself `k` steps later, for lags `k = 0` to
#' `max_lag`: the summed products of the deviations from `centre` that are `k`
#' steps apart, divided by `scale`.
#'
#' With the defaults, the deviations are from the series' own mean and `scale` is
#' their sum of squares, which is the estimator of [stats::acf()]. To pool
#' replicates of one process, give each replicate the mean of all replicates as
#' `centre`: a replicate's own mean pulls its estimate toward 0, the more so the
#' longer the autocorrelation lasts against the length of the series.
#' [autocorr_summary()] does this.
#'
#' The estimator divides the `n - k` products at lag `k` by all `n` squares,
#' which lowers it by a factor of about `(n - k) / n`. That is negligible on a
#' long series, but not on a few snapshots: at lag 1 of 7 snapshots, pooled over
#' replicates around their common mean, it averages about 0.82 of a true
#' correlation of 0.5. For a few snapshots of many replicates, use
#' [autocorr_snapshots()].
#'
#' @param x Numeric vector of equally spaced values. Non-finite values are
#'   dropped before the lags are taken, so they should only be at its ends.
#' @param max_lag Longest lag, in steps of the series.
#' @param centre Value the deviations are taken from. `NULL` (default) for the
#'   mean of `x`.
#' @param scale Denominator. `NULL` (default) for the sum of squared deviations
#'   from `centre`.
#'
#' @return A numeric vector of length `max_lag + 1`, lag 0 first. Lags at or
#'   beyond the length of the series are `NA`, and the whole vector is `NA` for a
#'   series of fewer than 3 finite values, a constant series (when `centre` is
#'   `NULL`), or a `scale` that is not positive.
#'
#' @export
#' @family autocorrelation
#' @examples
#' set.seed(1)
#' x <- as.numeric(stats::arima.sim(list(ar = 0.7), 500))
#' round(autocorr_acf(x, 5), 2) ## about 0.7^(0:5)
autocorr_acf <- function(x, max_lag, centre = NULL, scale = NULL) {
  x <- x[is.finite(x)]
  if (length(x) < 3L || (is.null(centre) && stats::sd(x) == 0)) {
    return(rep(NA_real_, max_lag + 1L))
  }
  d <- x - (if (is.null(centre)) mean(x) else centre)
  if (is.null(scale)) {
    scale <- sum(d^2)
  }
  if (!is.finite(scale) || scale <= 0) {
    return(rep(NA_real_, max_lag + 1L))
  }
  n <- length(x)
  k <- seq.int(0L, min(max_lag, n - 1L))
  rho <- vapply(k, function(l) sum(d[seq_len(n - l)] * d[seq_len(n - l) + l]) / scale, numeric(1))
  c(rho, rep(NA_real_, max_lag + 1L - length(rho)))
}

#' Integrated autocorrelation time
#'
#' The number of steps of a series that carry as much information as one
#' independent value: `tau = 1 + 2 * (rho[1] + rho[2] + ...)`, the sum of the
#' autocorrelations at lags 1 and up. A long series of `n` values is worth about
#' `n / tau` independent ones. White noise has `tau = 1`; an AR(1) process with
#' coefficient `phi` has `tau = (1 + phi) / (1 - phi)`.
#'
#' A sample autocorrelation function is noise at long lags, and summing all of it
#' adds that noise up. The sum here is Geyer's (1992) initial positive sequence
#' estimator: the autocorrelations are summed in pairs (lags 0 and 1, 2 and 3,
#' and so on), the pair sums are made non-increasing, and the sum stops before
#' the first pair sum that is not positive. It is still noisy on short series.
#'
#' @param rho Autocorrelation function, lag 0 first, as from [autocorr_acf()].
#'
#' @return The autocorrelation time, in steps of the series, at least 1. `NA` if
#'   `rho` has a missing value.
#'
#' @references Geyer, C. J. (1992). Practical Markov chain Monte Carlo.
#'   *Statistical Science* 7(4): 473-483. \doi{10.1214/ss/1177011137}
#'
#' @export
#' @family autocorrelation
#' @examples
#' autocorr_time(c(1, rep(0, 20))) ## white noise: 1
#' autocorr_time(0.5^(0:200)) ## AR(1) with phi = 0.5: (1 + 0.5) / (1 - 0.5) = 3
autocorr_time <- function(rho) {
  if (anyNA(rho)) {
    return(NA_real_)
  }
  if (length(rho) %% 2L) {
    rho <- c(rho, 0)
  }
  pairs <- cummin(rho[c(TRUE, FALSE)] + rho[c(FALSE, TRUE)])
  n <- which(pairs <= 0)[1L] - 1L
  if (is.na(n)) {
    n <- length(pairs)
  }
  max(1, -1 + 2 * sum(pairs[seq_len(n)]))
}

#' First lag at which an autocorrelation falls below a level
#'
#' @param rho Autocorrelation function, lag 0 first, as from [autocorr_acf()].
#' @param level Threshold.
#'
#' @return The first lag (0 for the first element of `rho`) whose
#'   autocorrelation is below `level`, as an integer. `NA` if none is, or if
#'   `rho` has a missing value.
#'
#' @export
#' @family autocorrelation
#' @examples
#' autocorr_lag_below(0.8^(0:30), 0.1) ## 0.8^11 = 0.086
autocorr_lag_below <- function(rho, level = 0.1) {
  k <- which(rho < level)[1L]
  if (anyNA(rho) || is.na(k)) NA_integer_ else k - 1L
}

#' Correlation between snapshots of replicated series
#'
#' The correlation between the values of a series `lag` snapshots apart, pooled
#' over replicates. It is meant for a few snapshots of many replicates, such as
#' the summary snapshots of NRV simulations, where estimators made for one long
#' series are biased.
#'
#' - `"ensemble"` (default): the deviation of each value from the mean of all
#'   replicates at the same snapshot, then the correlation of the deviations
#'   `lag` snapshots apart (their summed products over the square root of the
#'   product of their sums of squares), with the pairs of all replicates pooled.
#'   The snapshot means remove any trend the replicates share, such as a drift
#'   away from the initial conditions, and keep each replicate's lasting
#'   departure from them, which is the persistence to be measured. Estimating
#'   the snapshot means costs about 1 / (number of replicates) of the variance,
#'   so the estimator is nearly unbiased: on 15 replicates of 7 snapshots of a
#'   stationary AR(1) process whose adjacent snapshots have correlation `rho`,
#'   it averages 0.98 to 0.99 times `rho` (for `rho` from 0.1 to 0.9).
#' - `"acf"`: the pooled autocorrelation function of [autocorr_summary()]. The
#'   deviations are from the mean of all values, their products `lag` snapshots
#'   apart are summed over replicates and divided by the sum of all squares. It
#'   divides `n - lag` products by `n` squares, so at lag 1 of 7 snapshots of
#'   the same process it averages 0.82 times `rho` at 0.5 and 0.84 times at 0.9.
#'   A trend that all replicates share counts as correlation.
#'
#' Centring each replicate on its own mean, as a per-replicate Pearson
#' correlation does, is worse: with 7 snapshots that mean absorbs most of the
#' persistence, and a correlation of 0.5 averages about 0.2.
#'
#' @param x Numeric matrix with one row per replicate and one column per
#'   snapshot, in time order and equally spaced. A `data.frame` is converted
#'   with [base::as.matrix()]. A pair with a missing value is left out, and the
#'   snapshot means are those of the replicates with a value.
#' @param lag Lags, in snapshots (1 for adjacent snapshots).
#' @param method `"ensemble"` or `"acf"` (see Details).
#'
#' @return A numeric vector, one correlation per `lag`. `NA` for a lag with no
#'   pairs or a series with no variation around the means.
#'
#' @export
#' @family autocorrelation
#' @seealso [ess_ar1()] for the number of independent values that a correlation
#'   between snapshots leaves.
#' @examples
#' ## 15 replicates of 7 snapshots of an AR(1) process with snapshot correlation 0.6
#' set.seed(1)
#' x <- matrix(0, 15, 7)
#' x[, 1] <- stats::rnorm(15) / sqrt(1 - 0.6^2)
#' for (j in 2:7) x[, j] <- 0.6 * x[, j - 1] + stats::rnorm(15)
#' autocorr_snapshots(x, lag = 1:3)
#' autocorr_snapshots(x, lag = 1:3, method = "acf")
autocorr_snapshots <- function(x, lag = 1L, method = c("ensemble", "acf")) {
  method <- match.arg(method)
  x <- as.matrix(x)
  if (!is.numeric(x)) {
    stop("`x` must be numeric", call. = FALSE)
  }
  if (!is.numeric(lag) || anyNA(lag) || any(lag < 0) || any(lag != round(lag))) {
    stop("`lag` must be whole numbers of snapshots, 0 or more", call. = FALSE)
  }
  x[!is.finite(x)] <- NA_real_
  m <- ncol(x)
  if (identical(method, "ensemble")) {
    d <- x - matrix(colMeans(x, na.rm = TRUE), nrow(x), m, byrow = TRUE)
  } else {
    d <- x - mean(x, na.rm = TRUE)
    ss <- sum(d^2, na.rm = TRUE)
  }
  vapply(
    lag,
    function(k) {
      if (k >= m) {
        return(NA_real_)
      }
      a <- d[, seq_len(m - k), drop = FALSE]
      b <- d[, (1L + k):m, drop = FALSE]
      ok <- !is.na(a) & !is.na(b)
      if (!any(ok)) {
        return(NA_real_)
      }
      den <- if (identical(method, "ensemble")) sqrt(sum(a[ok]^2) * sum(b[ok]^2)) else ss
      if (!is.finite(den) || den <= 0) NA_real_ else sum(a[ok] * b[ok]) / den
    },
    numeric(1)
  )
}

.check_n <- function(n) {
  if (!is.numeric(n) || length(n) != 1L || is.na(n) || n < 0 || n != round(n)) {
    stop("`n` must be one whole number, 0 or more", call. = FALSE)
  }
  invisible(n)
}

## n / (1 + 2 * sum((1 - k / n) * rho_k)) for the autocorrelations `rho_k` at lags 1 to n - 1, clipped to
## [1, n]: the variance of the mean of n values is (1 + 2 * that sum) / n of one value's
.ess_finite <- function(n, rho_k) {
  if (n <= 1) {
    return(n)
  }
  k <- seq_len(n - 1L)
  den <- 1 + 2 * sum((1 - k / n) * rho_k[k])
  if (den <= 0) n else min(n, max(1, n / den))
}

#' Effective number of independent values in AR(1) series
#'
#' How many independent values `n_series` independent series (replicates) of
#' `n` values each are worth, when consecutive values of a series have
#' correlation `r` and the correlation decays as in an AR(1) process, to `r^k`
#' at `k` steps. It is the number of independent values whose mean would be as
#' precise as the mean of all `n * n_series` values.
#'
#' - `"finite"` (default): `n / (1 + 2 * sum((1 - k / n) * r^k))` per series,
#'   summed over `k = 1, ..., n - 1`. This is exact for the mean of `n`
#'   consecutive values of a stationary AR(1) process. It lies between 1 (`r`
#'   near 1, when a series is one value repeated) and `n` (no correlation).
#' - `"large_sample"`: `n * (1 - r) / (1 + r)` per series, the limit of the
#'   finite formula as `n` grows. It is close when `n` is much larger than the
#'   autocorrelation time `(1 + r) / (1 - r)`, and too low otherwise. For `r`
#'   above `(n - 1) / (n + 1)` (0.75 at `n = 7`), it is below 1 per series,
#'   fewer independent values than there are independent series. At 7 snapshots
#'   and a correlation of 0.9 it gives 0.37 per series, where the finite
#'   formula gives 1.25.
#'
#' A negative `r` is taken as 0, and an `r` above 0.999 as 0.999.
#'
#' @param n Number of values in each series (snapshots per replicate).
#' @param r Correlation between consecutive values; a vector gives one result
#'   per element.
#' @param n_series Number of independent series (replicates).
#' @param method `"finite"` or `"large_sample"` (see Details).
#'
#' @return A numeric vector the length of `r`: the effective number of
#'   independent values over all series. `NA` where `r` is `NA`.
#'
#' @export
#' @family autocorrelation
#' @seealso [ess_acf()] for a correlation that does not decay as in AR(1);
#'   [autocorr_snapshots()] to estimate `r` from replicated snapshots.
#' @examples
#' ess_ar1(7, c(0, 0.5, 0.9), n_series = 15)
#' ess_ar1(7, c(0, 0.5, 0.9), n_series = 15, method = "large_sample")
ess_ar1 <- function(n, r, n_series = 1, method = c("finite", "large_sample")) {
  method <- match.arg(method)
  .check_n(n)
  vapply(
    r,
    function(ri) {
      if (is.na(ri)) {
        return(NA_real_)
      }
      ri <- min(max(ri, 0), 0.999)
      per_series <- if (identical(method, "finite")) {
        .ess_finite(n, ri^seq_len(max(0, n - 1L)))
      } else {
        n * (1 - ri) / (1 + ri)
      }
      n_series * per_series
    },
    numeric(1)
  )
}

#' Effective number of independent values from an autocorrelation function
#'
#' As [ess_ar1()]'s finite formula, but for any autocorrelation function rather
#' than an AR(1) decay: `n / (1 + 2 * sum((1 - k / n) * rho[k]))` per series,
#' summed over lags `k = 1, ..., n - 1`, for independent series (replicates) of
#' `n` values that share the autocorrelation function `rho`. A sample
#' autocorrelation function need not be one a process can have, so the result
#' is clipped to between 1 and `n` per series.
#'
#' @param n Number of values in each series (snapshots per replicate).
#' @param rho Autocorrelation function, lag 0 first, to at least lag `n - 1`
#'   (lag 0 is not used). For instance `c(1, autocorr_snapshots(x, 1:(n - 1)))`.
#' @param n_series Number of independent series (replicates).
#'
#' @return The effective number of independent values over all series. `NA` if
#'   `rho` has a missing value at lags 1 to `n - 1`.
#'
#' @export
#' @family autocorrelation
#' @examples
#' ## equal to ess_ar1() for an AR(1) decay
#' ess_acf(7, 0.6^(0:6), n_series = 15)
#' ess_ar1(7, 0.6, n_series = 15)
ess_acf <- function(n, rho, n_series = 1) {
  .check_n(n)
  if (length(rho) < n) {
    stop(
      "`rho` must run from lag 0 to at least lag n - 1 (",
      n,
      " values); it has ",
      length(rho),
      call. = FALSE
    )
  }
  rho_k <- rho[-1L][seq_len(max(0, n - 1L))]
  if (anyNA(rho_k)) {
    return(NA_real_)
  }
  n_series * .ess_finite(n, rho_k)
}

#' Autocorrelation of replicated annual landscape series
#'
#' How long the landscape series of replicated simulations stay correlated, and
#' how many independent samples the summary snapshots are worth. Each table
#' holds one replicate's series recorded every year (or every step), such as the
#' proportion of old forest. The interval at which summary snapshots are nearly
#' independent is the autocorrelation time of these series.
#'
#' The replicates are taken as realisations of one process, so each series'
#' autocorrelation function (ACF) is taken around the mean of all replicates and
#' years of `period`, and the variance is pooled over the replicates: a
#' replicate's own mean would pull the ACF toward 0, the more so the longer the
#' autocorrelation time is against the series. A short series still biases it
#' toward 0, so the estimates are lower bounds, and `nYears` says how much to
#' trust them. If the replicates' means differ, the common mean counts the
#' difference as autocorrelation, which errs toward more thinning.
#'
#' Per series, the result has
#' - `phi`: the pooled lag-1 autocorrelation, clipped to `[0, 0.999]`, and
#'   `tau`, the autocorrelation time of the AR(1) decay it implies,
#'   `(1 + phi) / (1 - phi)` (no curve is fitted);
#' - `ar1Misfit`: the largest absolute difference between the pooled ACF at lags
#'   1 to 50 and `phi^lag`, near 0 for an exponential decay and large for an
#'   oscillating series;
#' - `r`, `rMin`, `rMax`: the ACF at lag `interval`, pooled, and of the lowest
#'   and highest replicate (each replicate's own correlation around the common
#'   mean);
#' - `effectiveSnapshots`: the snapshots of all replicates, times
#'   `(1 - r) / (1 + r)` ([ess_ar1()]'s large-sample formula, too low for a few
#'   snapshots per replicate), and `effectiveSnapshotsFinite`, from its finite
#'   formula;
#' - `tauGeyer`: [autocorr_time()] of the pooled ACF, for comparison (noisy for
#'   short series);
#' - `lagBelow0.1`: the first lag at which the pooled ACF is below 0.1.
#'
#' No lag reaches beyond the shortest replicate's years in `period` less one:
#' there is no correlation to measure past that. A lag that is cut away is `NA`
#' (`r`, when `interval` is), and the other columns use the lags that remain.
#'
#' @param tables List of `data.frame`s, one per replicate, each with a `time`
#'   column and one numeric column per series. A series not in every table is
#'   left out (see `absent` in the result).
#' @param period The years analysed, `c(from, to)`: the NRV years.
#' @param max_lag Longest lag of the ACF, in years. The ACF runs to the largest
#'   of `max_lag`, `interval` and 50, within the years available.
#' @param interval The summary interval: the years between snapshots.
#' @param snapshot_times The summary times in `period`. The default assumes the
#'   first snapshot is at the start of `period`; a caller whose snapshots are
#'   on another grid (an NRV window that starts between two snapshots, say)
#'   passes them.
#'
#' @return A list:
#' - `fit`: a `data.frame`, one row per series, with the columns above;
#' - `acf`: the empirical ACF per `series`, `rep` (the table's position, or
#'   `"pooled"`) and `lag`;
#' - `fitted`: the `phi^lag` curve per series and lag;
#' - `thin`: the recommended snapshot spacing, `ceiling()` of the largest `tau`,
#'   and `limiting`, the series with that `tau` (both `NA` when no series has
#'   one);
#' - `interval`; `nSnapshots`, the snapshots over all replicates; `nYears`, the
#'   shortest replicate's years in `period`;
#' - `absent`: the series not in every table; `undefined`: those with no `tau`,
#'   either `constant` over `period` or with `tooFew` (fewer than 3) values in
#'   some replicate's years of `period`.
#'
#' @export
#' @family autocorrelation
#' @examples
#' set.seed(1)
#' tables <- lapply(1:5, function(i) {
#'   data.frame(time = 1:300, x = as.numeric(stats::arima.sim(list(ar = 0.9), 300)))
#' })
#' res <- autocorr_summary(tables, period = c(1, 300), max_lag = 100, interval = 50)
#' res$fit[c("series", "phi", "tau", "r", "effectiveSnapshots", "effectiveSnapshotsFinite")]
autocorr_summary <- function(
  tables,
  period,
  max_lag,
  interval,
  snapshot_times = seq(period[1L], period[2L], by = interval)
) {
  period <- range(period)
  tol <- 1e-8
  if (any(snapshot_times < period[1L] - tol | snapshot_times > period[2L] + tol)) {
    stop(
      "`snapshot_times` must lie within `period` (",
      period[1L],
      "-",
      period[2L],
      ")",
      call. = FALSE
    )
  }
  nSnapshotsPerRep <- length(unique(snapshot_times))
  series <- setdiff(Reduce(intersect, lapply(tables, names)), "time")
  absent <- setdiff(Reduce(union, lapply(tables, names)), c(series, "time"))
  inPeriod <- lapply(tables, function(d) {
    d[d$time >= period[1L] & d$time <= period[2L], , drop = FALSE]
  })
  nYears <- min(vapply(inPeriod, nrow, integer(1)))
  lagMax <- max(1L, min(max(max_lag, interval, 50L), nYears - 1L))
  nReps <- length(tables)
  vals <- lapply(series, function(s) lapply(inPeriod, function(d) d[[s]][is.finite(d[[s]])]))
  names(vals) <- series
  isConstant <- vapply(vals, function(x) length(unique(unlist(x))) < 2L, logical(1))
  tooFew <- vapply(vals, function(x) min(lengths(x)) < 3L, logical(1)) & !isConstant
  ## per series: each rep's ACF is its lagged products over its own sum of squares around the common mean (a
  ## correlation, 1 at lag 0); the pooled ACF is the summed lagged products over the summed squares
  perSeries <- lapply(series, function(s) {
    x <- vals[[s]]
    m <- mean(unlist(x))
    ssAll <- sum(unlist(lapply(x, function(v) (v - m)^2)))
    undefined <- isConstant[[s]] || tooFew[[s]]
    na <- rep(NA_real_, lagMax + 1L)
    rhos <- lapply(x, function(v) {
      if (undefined) na else autocorr_acf(v, lagMax, centre = m, scale = sum((v - m)^2))
    })
    pooledRho <- if (undefined) {
      na
    } else {
      Reduce(`+`, lapply(x, autocorr_acf, lagMax, centre = m, scale = ssAll))
    }
    list(
      reps = do.call(
        rbind,
        lapply(seq_len(nReps), function(i) {
          data.frame(
            series = s,
            rep = as.character(i),
            lag = seq_along(rhos[[i]]) - 1L,
            acf = rhos[[i]]
          )
        })
      ),
      pooled = data.frame(
        series = s,
        rep = "pooled",
        lag = seq_along(pooledRho) - 1L,
        acf = pooledRho
      )
    )
  })
  acfs <- do.call(rbind, lapply(perSeries, `[[`, "reps"))
  pooledAcf <- do.call(rbind, lapply(perSeries, `[[`, "pooled"))
  acfAt <- function(d, lag) {
    v <- d$acf[d$lag == lag]
    if (length(v)) v else NA_real_
  }
  misfitLags <- seq_len(min(50L, lagMax))
  fit <- do.call(
    rbind,
    lapply(series, function(s) {
      reps <- acfs[acfs$series == s, , drop = FALSE]
      pool <- pooledAcf[pooledAcf$series == s, , drop = FALSE]
      phi <- min(max(acfAt(pool, 1L), 0), 0.999)
      rInt <- vapply(split(reps, reps$rep), acfAt, numeric(1), lag = interval)
      r <- acfAt(pool, interval)
      data.frame(
        series = s,
        phi = phi,
        tau = (1 + phi) / (1 - phi),
        ar1Misfit = max(abs(pool$acf[pool$lag %in% misfitLags] - phi^misfitLags)),
        r = r,
        rMin = if (all(is.na(rInt))) NA_real_ else min(rInt, na.rm = TRUE),
        rMax = if (all(is.na(rInt))) NA_real_ else max(rInt, na.rm = TRUE),
        effectiveSnapshots = ess_ar1(nSnapshotsPerRep, r, nReps, method = "large_sample"),
        effectiveSnapshotsFinite = ess_ar1(nSnapshotsPerRep, r, nReps),
        tauGeyer = autocorr_time(pool$acf),
        lagBelow0.1 = autocorr_lag_below(pool$acf, 0.1)
      )
    })
  )
  fitted <- merge(fit[c("series", "phi")], data.frame(lag = 0:lagMax))
  fitted$acf <- fitted$phi^fitted$lag
  hasTau <- any(!is.na(fit$tau))
  limiting <- if (hasTau) fit$series[which.max(fit$tau)] else NA_character_
  list(
    acf = rbind(acfs, pooledAcf),
    fitted = fitted,
    fit = fit,
    thin = if (hasTau) ceiling(max(fit$tau, na.rm = TRUE)) else NA_real_,
    limiting = limiting,
    interval = interval,
    nSnapshots = nSnapshotsPerRep * nReps,
    nYears = nYears,
    absent = absent,
    undefined = fit$series[is.na(fit$tau)],
    constant = series[isConstant],
    tooFew = series[tooFew]
  )
}
