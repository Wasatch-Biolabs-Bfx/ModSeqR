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
# Matches the signature pattern of .calc_diff_wilcox / .calc_diff_fisher
# so it integrates directly into calc_mod_diff()'s switch() statement.
# ===========================================================================
.calc_diff_betabin <- function(in_dat)
{
  # Determine grouping columns without collecting any data
  group_vars <- setdiff(
    colnames(in_dat),
    c("sample_name", "exp_group", "num_calls", "mod_counts", "num_sites")
  )

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

  # If chrom is a grouping column, process one chromosome at a time so only
  # one chromosome's rows are in R memory at once.
  if ("chrom" %in% group_vars) {
    chroms <- dplyr::pull(dplyr::distinct(dplyr::select(slim, chrom)))

    chunks <- lapply(chroms, function(chr) {
      slim |>
        dplyr::filter(chrom == chr) |>
        dplyr::collect() |>
        .run_lrt()
    })

    do.call(rbind, chunks)
  } else {
    dplyr::collect(slim) |> .run_lrt()
  }
}
