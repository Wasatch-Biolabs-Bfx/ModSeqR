#' Beta-Binomial Differential Modification Test
#'
#' Internal function called by \code{calc_mod_diff()} when \code{calc_type = "beta_bin"}.
#' Implements a beta-binomial model that accounts for biological variability
#' (overdispersion) across replicates, making it appropriate for small-sample
#' designs (2-4 replicates per group) where the Wilcoxon test lacks power and
#' the pooled Fisher test ignores replicate variability.
#'
#' @section Model:
#' At each genomic locus, each sample's modified-read count is modelled as:
#' \deqn{X_i \sim \mathrm{BetaBinomial}(n_i,\; \alpha,\; \beta)}
#' where \eqn{n_i} is the total read count for sample \eqn{i}, and
#' \eqn{\alpha, \beta} are the beta distribution shape parameters shared
#' across samples within a group.
#'
#' We reparameterise to \eqn{\mu = \alpha / (\alpha + \beta)} (mean modification
#' fraction) and \eqn{\phi = 1 / (\alpha + \beta + 1)} (overdispersion, 0-1).
#'
#' A likelihood-ratio test compares:
#' \itemize{
#'   \item \strong{H0 (null):} one shared \eqn{\mu} for both groups
#'   \item \strong{H1 (alt):}  separate \eqn{\mu_{case}} and \eqn{\mu_{control}}
#' }
#' Both models share a single overdispersion parameter \eqn{\phi}.
#' The test statistic \eqn{-2 \log(\Lambda)} is compared to a \eqn{\chi^2_1}
#' distribution to obtain a p-value.
#'
#' @param in_dat A lazy \code{tbl} (DuckDB-backed) with columns:
#'   \code{sample_name}, grouping columns (e.g. \code{chrom}, \code{start},
#'   \code{end}, \code{region_name}), \code{num_calls}, \code{mod_counts},
#'   and \code{exp_group} (\code{"case"} or \code{"control"}).
#'
#' @return A data.frame with one row per locus containing:
#'   \describe{
#'     \item{grouping columns}{e.g. chrom, start, end, region_name}
#'     \item{num_samples_case / num_samples_control}{sample counts per group}
#'     \item{num_calls_case / num_calls_control}{total reads per group}
#'     \item{mod_counts_case / mod_counts_control}{total modified reads per group}
#'     \item{mod_frac_case / mod_frac_control}{mean per-sample mod fraction}
#'     \item{meth_diff}{mod_frac_case - mod_frac_control}
#'     \item{overdispersion}{estimated phi}
#'     \item{p_val}{likelihood-ratio test p-value}
#'   }
#'
#' @details
#' The beta-binomial log-likelihood for a single observation is:
#' \deqn{\log P(x | n, \mu, \phi) =
#'   \log\binom{n}{x}
#'   + \sum_{j=0}^{x-1} \log(\mu(1-\phi)/\phi + j)
#'   + \sum_{j=0}^{n-x-1} \log((1-\mu)(1-\phi)/\phi + j)
#'   - \sum_{j=0}^{n-1} \log((1-\phi)/\phi + j)}
#'
#' When \eqn{\phi \to 0}, the beta-binomial reduces to a standard binomial.
#' Optimisation uses \code{stats::optim(method = "L-BFGS-B")} with logit/log
#' transforms for numerical stability.
#'
#' No additional package dependencies are required beyond base R.
#'
#' @keywords internal


# ===========================================================================
# Beta-binomial log-likelihood (vectorised over observations)
# ===========================================================================
# Parameters:
#   x   - vector of modified counts
#   n   - vector of total counts
#   mu  - mean modification fraction (scalar, 0 < mu < 1)
#   phi - overdispersion            (scalar, 0 < phi < 1)
#
# Uses the lbeta() formulation for speed and numerical stability:
#   log P(x | n, mu, phi) = lchoose(n, x)
#                         + lbeta(x + alpha, n - x + beta)
#                         - lbeta(alpha, beta)
# where alpha = mu * (1 - phi) / phi,  beta = (1 - mu) * (1 - phi) / phi

.bb_loglik <- function(x, n, mu, phi)
{
  # Guard against boundary values
  mu  <- pmax(pmin(mu, 1 - 1e-12), 1e-12)
  phi <- pmax(pmin(phi, 1 - 1e-6), 1e-6)

  alpha <- mu * (1 - phi) / phi
  beta  <- (1 - mu) * (1 - phi) / phi

  ll <- lchoose(n, x) +
    lbeta(x + alpha, n - x + beta) -
    lbeta(alpha, beta)

  sum(ll)
}


# ===========================================================================
# Fit the NULL model: one shared mu, one shared phi across all samples
# ===========================================================================
.bb_fit_null <- function(x, n)
{
  # Starting values from method of moments
  p_hat <- sum(x) / sum(n)
  p_hat <- pmax(pmin(p_hat, 0.999), 0.001)

  # Starting phi from variance of per-sample fractions
  fracs <- x / pmax(n, 1)
  if (length(fracs) > 1 && stats::var(fracs) > 0) {
    v <- stats::var(fracs)
    p_bar <- mean(fracs)
    n_bar <- mean(n)
    # Method of moments phi estimate
    phi_start <- max((v - p_bar * (1 - p_bar) / n_bar) /
                       (p_bar * (1 - p_bar) * (1 - 1/n_bar)), 0.01)
    phi_start <- min(phi_start, 0.99)
  } else {
    phi_start <- 0.01
  }

  # Optimise on logit(mu) and log(phi/(1-phi)) for unconstrained space
  start <- c(stats::qlogis(p_hat), stats::qlogis(phi_start))

  neg_ll <- function(par) {
    mu  <- stats::plogis(par[1])
    phi <- stats::plogis(par[2])
    -.bb_loglik(x, n, mu, phi)
  }

  fit <- tryCatch(
    stats::optim(start, neg_ll, method = "L-BFGS-B"),
    error = function(e) list(par = start, value = neg_ll(start),
                             convergence = 1)
  )

  list(
    mu    = stats::plogis(fit$par[1]),
    phi   = stats::plogis(fit$par[2]),
    loglik = -fit$value,
    convergence = fit$convergence
  )
}


# ===========================================================================
# Fit the ALTERNATIVE model: separate mu per group, shared phi
# ===========================================================================
.bb_fit_alt <- function(x_case, n_case, x_ctrl, n_ctrl)
{
  # Starting values per group
  mu_case_start <- sum(x_case) / max(sum(n_case), 1)
  mu_ctrl_start <- sum(x_ctrl) / max(sum(n_ctrl), 1)
  mu_case_start <- pmax(pmin(mu_case_start, 0.999), 0.001)
  mu_ctrl_start <- pmax(pmin(mu_ctrl_start, 0.999), 0.001)

  # Shared phi from pooled variance
  fracs <- c(x_case / pmax(n_case, 1), x_ctrl / pmax(n_ctrl, 1))
  if (length(fracs) > 1 && stats::var(fracs) > 0) {
    v <- stats::var(fracs)
    p_bar <- mean(fracs)
    n_bar <- mean(c(n_case, n_ctrl))
    phi_start <- max((v - p_bar * (1 - p_bar) / n_bar) /
                       (p_bar * (1 - p_bar) * (1 - 1/n_bar)), 0.01)
    phi_start <- min(phi_start, 0.99)
  } else {
    phi_start <- 0.01
  }

  start <- c(stats::qlogis(mu_case_start),
             stats::qlogis(mu_ctrl_start),
             stats::qlogis(phi_start))

  neg_ll <- function(par) {
    mu_c <- stats::plogis(par[1])
    mu_t <- stats::plogis(par[2])
    phi  <- stats::plogis(par[3])
    -(.bb_loglik(x_case, n_case, mu_c, phi) +
        .bb_loglik(x_ctrl, n_ctrl, mu_t, phi))
  }

  fit <- tryCatch(
    stats::optim(start, neg_ll, method = "L-BFGS-B"),
    error = function(e) list(par = start, value = neg_ll(start),
                             convergence = 1)
  )

  list(
    mu_case = stats::plogis(fit$par[1]),
    mu_ctrl = stats::plogis(fit$par[2]),
    phi     = stats::plogis(fit$par[3]),
    loglik  = -fit$value,
    convergence = fit$convergence
  )
}


# ===========================================================================
# Likelihood-ratio test for one locus
# ===========================================================================
# ===========================================================================
# Empirical-Bayes beta-binomial GLM with FIXED dispersion (calc_type = "eb_beta_bin")
# ===========================================================================
# These power the EB path (see calc_diff_eb.R). Unlike .bb_loglik / .bb_fit_*,
# the dispersion `rho` (intra-class correlation, = phi above) is supplied FIXED
# at the empirical-Bayes shrunk value and only the mean coefficients of a
# logit-link GLM are optimised: logit(mu_i) = X_i^T beta, with per-sample
#   alpha_i = mu_i (1 - rho) / rho,   beta_i = (1 - mu_i)(1 - rho) / rho.
# Matches FerruMod ModDiff's calc_mod_diff_eb.

# Fixed-rho beta-binomial log-likelihood for a GLM mean (vectorised over samples)
.bb_loglik_glm <- function(x, n, coef, X, rho)
{
  eta <- as.numeric(X %*% coef)
  mu  <- stats::plogis(eta)
  mu  <- pmax(pmin(mu, 1 - 1e-9), 1e-9)
  rho <- min(max(rho, 1e-6), 1 - 1e-6)

  a <- mu * (1 - rho) / rho
  b <- (1 - mu) * (1 - rho) / rho

  sum(lchoose(n, x) + lbeta(x + a, n - x + b) - lbeta(a, b))
}

# Fit the GLM coefficients with rho held fixed. Warm-start from a weighted
# binomial GLM (read counts as weights), then refine under the beta-binomial
# likelihood with BOX-CONSTRAINED L-BFGS-B.
#
# The coefficients are bounded to +/-`.BB_COEF_BOUND` (logit scale). This is
# essential: .bb_loglik_glm clamps mu to [1e-9, 1-1e-9], so the log-likelihood
# is FLAT once any fitted mu saturates. An unbounded optimiser (BFGS) can wander
# onto that plateau, "converge" (zero gradient) at a garbage coefficient like
# -386, and report a far-too-low loglik -- which, for a nested null model,
# inflates the LRT to spurious p ~ 1e-200. Bounding at +/-15 keeps every fitted
# mu inside (3e-7, 1-3e-7) -- well clear of the clamp -- so the surface stays
# smooth and the fit lands at the true optimum near the glm.fit warm start.
.BB_COEF_BOUND <- 15

.bb_fit_glm <- function(x, n, X, rho)
{
  np <- ncol(X)
  lo <- rep(-.BB_COEF_BOUND, np); hi <- rep(.BB_COEF_BOUND, np)

  start <- tryCatch({
    fit0 <- suppressWarnings(
      stats::glm.fit(X, x / pmax(n, 1), weights = pmax(n, 1),
                     family = stats::binomial()))
    cf <- fit0$coefficients
    cf[!is.finite(cf)] <- 0
    cf
  }, error = function(e) rep(0, np))
  start <- pmin(pmax(start, lo), hi)

  neg_ll <- function(par) -.bb_loglik_glm(x, n, par, X, rho)

  fit <- tryCatch(
    stats::optim(start, neg_ll, method = "L-BFGS-B", lower = lo, upper = hi),
    error = function(e) tryCatch(
      stats::optim(start, neg_ll, method = "Nelder-Mead"),
      error = function(e2) list(par = start, value = neg_ll(start),
                                convergence = 1)))

  # Guard against a fallback (or a pathological step) returning a worse fit than
  # the warm start: never report a loglik below the start's.
  start_nll <- neg_ll(start)
  if (!is.finite(fit$value) || fit$value > start_nll) {
    fit$par <- start; fit$value <- start_nll
  }
  list(coef = fit$par, loglik = -fit$value, convergence = fit$convergence)
}

# LRT for the group effect at one locus, dispersion fixed at `rho`.
# `grp` is a 0/1 case indicator; `covmat` is a (samples x covariates) numeric
# matrix or NULL. Full model = intercept + group + covariates; null drops group.
.eb_bb_lrt <- function(x, n, grp, covmat, rho)
{
  cov_ok <- if (is.null(covmat)) rep(TRUE, length(x)) else
    rowSums(!is.finite(covmat)) == 0
  ok <- is.finite(x) & is.finite(n) & n > 0 & is.finite(grp) & cov_ok

  na_out <- list(p_val = NA_real_, coef_group = NA_real_)
  if (sum(ok) < 3) return(na_out)
  grp_ok <- grp[ok]
  if (length(unique(grp_ok)) < 2) return(na_out)
  if (is.na(rho) || rho <= 0 || rho >= 1) return(na_out)

  x_ok <- x[ok]; n_ok <- n[ok]
  cov_ok_mat <- if (is.null(covmat)) NULL else covmat[ok, , drop = FALSE]

  X_null <- cbind(1, cov_ok_mat)
  X_full <- cbind(1, grp_ok, cov_ok_mat)
  # Need more samples than parameters for a meaningful fit.
  if (nrow(X_full) <= ncol(X_full)) return(na_out)

  null_fit <- .bb_fit_glm(x_ok, n_ok, X_null, rho)
  full_fit <- .bb_fit_glm(x_ok, n_ok, X_full, rho)

  lr <- max(2 * (full_fit$loglik - null_fit$loglik), 0)
  list(
    p_val      = stats::pchisq(lr, df = 1, lower.tail = FALSE),
    coef_group = full_fit$coef[2]   # X_full col 2 is the group indicator
  )
}


.bb_lrt <- function(x_case, n_case, x_ctrl, n_ctrl)
{
  # If all counts are zero on either side, can't test
  if (sum(n_case) == 0 || sum(n_ctrl) == 0) {
    return(list(p_val = NA_real_, mu_case = NA_real_, mu_ctrl = NA_real_,
                phi = NA_real_))
  }

  # Null: shared mu across both groups
  null_fit <- .bb_fit_null(c(x_case, x_ctrl), c(n_case, n_ctrl))

  # Alt: separate mu per group
  alt_fit  <- .bb_fit_alt(x_case, n_case, x_ctrl, n_ctrl)

  # LRT statistic: -2 * (loglik_null - loglik_alt)
  lrt_stat <- -2 * (null_fit$loglik - alt_fit$loglik)
  lrt_stat <- max(lrt_stat, 0)  # numerical floor

  # 1 df (one extra parameter: mu_case vs mu_ctrl)
  p_val <- stats::pchisq(lrt_stat, df = 1, lower.tail = FALSE)

  list(
    p_val   = p_val,
    mu_case = alt_fit$mu_case,
    mu_ctrl = alt_fit$mu_ctrl,
    phi     = alt_fit$phi
  )
}


# ===========================================================================
# Main entry point: .calc_diff_betabin()
#
# Called once per chromosome by .calc_diff_stream_by_chrom() (in calc_mod_diff.R),
# which handles the chromosome loop and streams each chromosome's result to the
# output table. This function processes the single (already chrom-filtered) slice
# it is handed and returns a locus-level data.frame.
# ===========================================================================
.calc_diff_betabin <- function(in_dat, group_vars)
{
  # Only pull the columns the LRT actually needs — skip all pre-computed
  # fraction/count columns that may be wide in position/window tables.
  needed <- c(group_vars, "sample_name", "exp_group", "num_calls", "mod_counts")
  slim   <- dplyr::select(in_dat, dplyr::any_of(needed))

  # Helper: run the LRT on one in-memory data frame and return a summarised tibble
  .run_lrt <- function(dat) {
    dat |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
      dplyr::summarise(
        num_samples_case    = sum(exp_group == "case"),
        num_samples_control = sum(exp_group == "control"),

        num_calls_case     = sum(num_calls[exp_group == "case"],    na.rm = TRUE),
        num_calls_control  = sum(num_calls[exp_group == "control"], na.rm = TRUE),

        mod_counts_case    = sum(mod_counts[exp_group == "case"],    na.rm = TRUE),
        mod_counts_control = sum(mod_counts[exp_group == "control"], na.rm = TRUE),

        mod_frac_case = mean(
          mod_counts[exp_group == "case"] / pmax(num_calls[exp_group == "case"], 1),
          na.rm = TRUE
        ),
        mod_frac_control = mean(
          mod_counts[exp_group == "control"] / pmax(num_calls[exp_group == "control"], 1),
          na.rm = TRUE
        ),

        {
          x_case <- mod_counts[exp_group == "case"]
          n_case <- num_calls[exp_group == "case"]
          x_ctrl <- mod_counts[exp_group == "control"]
          n_ctrl <- num_calls[exp_group == "control"]

          res <- .bb_lrt(x_case, n_case, x_ctrl, n_ctrl)

          data.frame(
            meth_diff      = res$mu_case - res$mu_ctrl,
            overdispersion = res$phi,
            p_val          = res$p_val
          )
        },

        .groups = "drop"
      )
  }

  dat <- dplyr::collect(slim)
  if (nrow(dat) == 0) return(data.frame())
  .run_lrt(dat)
}


# ===========================================================================
# Main entry point for the empirical-Bayes path: .calc_diff_eb_betabin()
#
# Mirrors .calc_diff_betabin() but runs the fixed-rho GLM LRT (.eb_bb_lrt). The
# input slice carries the per-locus shrunk dispersion in `_rho_shrunk` (added by
# .eb_shrunk_dispersion, constant within a locus) and any covariate columns.
# Output schema matches beta_bin (plus `coef_group`) so collapse_mod_windows()
# works unchanged. Called per chromosome by .calc_diff_eb_run() (calc_diff_eb.R).
# ===========================================================================
.calc_diff_eb_betabin <- function(in_dat, group_vars, covariate_cols = character(0))
{
  needed <- c(group_vars, "sample_name", "exp_group", "num_calls", "mod_counts",
              "_rho_shrunk", covariate_cols)
  slim <- dplyr::select(in_dat, dplyr::any_of(needed))

  dat <- dplyr::collect(slim)
  if (nrow(dat) == 0) return(data.frame())

  dat |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_vars))) |>
    dplyr::summarise(
      num_samples_case    = sum(exp_group == "case"),
      num_samples_control = sum(exp_group == "control"),

      num_calls_case     = sum(num_calls[exp_group == "case"],    na.rm = TRUE),
      num_calls_control  = sum(num_calls[exp_group == "control"], na.rm = TRUE),

      mod_counts_case    = sum(mod_counts[exp_group == "case"],    na.rm = TRUE),
      mod_counts_control = sum(mod_counts[exp_group == "control"], na.rm = TRUE),

      mod_frac_case = mean(
        mod_counts[exp_group == "case"] / pmax(num_calls[exp_group == "case"], 1),
        na.rm = TRUE),
      mod_frac_control = mean(
        mod_counts[exp_group == "control"] / pmax(num_calls[exp_group == "control"], 1),
        na.rm = TRUE),

      meth_diff = mean(
        mod_counts[exp_group == "case"] / pmax(num_calls[exp_group == "case"], 1),
        na.rm = TRUE) -
        mean(
          mod_counts[exp_group == "control"] / pmax(num_calls[exp_group == "control"], 1),
          na.rm = TRUE),

      {
        x   <- mod_counts
        n   <- num_calls
        grp <- as.integer(exp_group == "case")
        rho <- `_rho_shrunk`[1]
        covmat <- if (length(covariate_cols) > 0)
          as.matrix(dplyr::pick(dplyr::all_of(covariate_cols))) else NULL

        res <- .eb_bb_lrt(x, n, grp, covmat, rho)

        data.frame(
          overdispersion = rho,
          coef_group     = res$coef_group,
          p_val          = res$p_val
        )
      },

      .groups = "drop"
    )
}
