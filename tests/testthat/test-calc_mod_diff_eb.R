# Tests for 1.3.2: empirical-Bayes beta-binomial (eb_beta_bin) -- the SQL
# shrunk-dispersion pass and the per-locus fixed-rho GLM LRT, including covariate
# adjustment and parallel == serial.

.mk_db <- function(con, dbfile) {
  env        <- new.env(parent = emptyenv())
  env$con    <- con
  env$db_key <- normalizePath(dbfile, mustWork = FALSE)
  obj <- list(db_file = dbfile, current_table = NULL, last_result = NULL,
              config = list(memory_limit = NULL, temp_dir = NULL, threads = NULL),
              .conn_env = env)
  class(obj) <- "mod_db"
  obj
}

# windows fixture: several samples/group, two chroms, real signal at every 4th locus
.eb_windows_df <- function(n_case = 8, n_ctrl = 10, n_loci = 40, seed = 11) {
  set.seed(seed)
  cases <- paste0("case", seq_len(n_case)); ctrls <- paste0("ctrl", seq_len(n_ctrl))
  do.call(rbind, lapply(seq_len(n_loci), function(i) {
    mu_c <- runif(1, 0.3, 0.7)
    mu_t <- if (i %% 4 == 0) min(max(mu_c + 0.30, 0.05), 0.95) else mu_c
    dc <- pmax(rpois(n_case, 8), 1); dt <- pmax(rpois(n_ctrl, 8), 1)
    data.frame(sample_name = c(cases, ctrls),
               chrom = paste0("chr", 1 + (i %% 2)),
               start = i * 100L, end = i * 100L + 1L, num_sites = 5L,
               num_calls = c(dc, dt),
               m_counts  = c(rbinom(n_case, dc, mu_c), rbinom(n_ctrl, dt, mu_t)),
               stringsAsFactors = FALSE)
  }))
}

.run_eb <- function(df, ncores = 1L, covariates = NULL, sample_meta = NULL,
                    eb_df_prior = 10,
                    cases = paste0("case", 1:8), controls = paste0("ctrl", 1:10)) {
  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "t.mod.db")
  con <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "windows", df)
  obj <- .mk_db(con, dbfile)
  out <- NULL
  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    {
      d <- ModSeqR::calc_mod_diff(obj, input_table = "windows", output_table = "d",
                                  cases = cases, controls = controls, mod_type = "m",
                                  calc_type = "eb_beta_bin", min_samples = 2L,
                                  n_cores = ncores, overwrite = TRUE,
                                  covariates = covariates, sample_meta = sample_meta,
                                  eb_df_prior = eb_df_prior)
      out <- ModSeqR::get_mod_table(d, "d")
    })
  out[order(out$chrom, out$start), ]
}

test_that("eb_beta_bin produces valid p-values, overdispersion in (0,1), coef_group", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  res <- .run_eb(.eb_windows_df())
  expect_true(nrow(res) > 0)
  expect_true(all(c("meth_diff", "overdispersion", "coef_group",
                    "p_val", "p_adjust") %in% names(res)))
  ok <- !is.na(res$p_val)
  expect_true(all(res$p_val[ok] >= 0 & res$p_val[ok] <= 1))
  rho <- res$overdispersion[!is.na(res$overdispersion)]
  expect_true(all(rho > 0 & rho < 1))   # ICC scale, shrunk
})

test_that("eb_beta_bin is calibrated under the null and powered at planted signal", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  res <- .run_eb(.eb_windows_df())
  signal <- (as.integer(res$start / 100L) %% 4L) == 0L
  # Planted loci (every 4th) should have much smaller p than the null loci.
  expect_lt(mean(res$p_val[signal],  na.rm = TRUE),
            mean(res$p_val[!signal], na.rm = TRUE))
  # Null loci should not be systematically tiny.
  expect_gt(median(res$p_val[!signal], na.rm = TRUE), 0.2)
})

test_that("low-methylation null data does NOT produce spurious tiny p-values", {
  # Regression test for the optimizer-divergence bug: at low methylation (mu~0),
  # an unbounded optimiser could run the null-model intercept off to -386 onto
  # the mu-clamp plateau, reporting a far-too-low loglik and inflating the LRT to
  # p ~ 1e-200 for loci with ~zero effect. With bounded coefficients the null
  # data must stay calibrated (no absurd p-values, frac p<0.01 not inflated).
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  set.seed(7)
  n_case <- 19; n_ctrl <- 51; n_loci <- 120
  cs <- paste0("case", seq_len(n_case)); ct <- paste0("ctrl", seq_len(n_ctrl))
  df <- do.call(rbind, lapply(seq_len(n_loci), function(i) {
    mu <- runif(1, 0.01, 0.05)                 # low methylation, NO group effect
    dc <- pmax(rpois(n_case, 30), 1); dt <- pmax(rpois(n_ctrl, 30), 1)  # varied, high depth
    data.frame(sample_name = c(cs, ct), chrom = "chr1",
               start = i * 200L, end = i * 200L + 1L, num_sites = 5L,
               num_calls = c(dc, dt),
               m_counts  = c(rbinom(n_case, dc, mu), rbinom(n_ctrl, dt, mu)),
               stringsAsFactors = FALSE)
  }))
  res <- .run_eb(df, cases = cs, controls = ct)
  expect_true(all(res$p_val[!is.na(res$p_val)] >= 0 & res$p_val[!is.na(res$p_val)] <= 1))
  # No catastrophic false positives from optimizer divergence.
  expect_gt(min(res$p_val, na.rm = TRUE), 1e-6)
  # Null data: not wildly anti-conservative.
  expect_lt(mean(res$p_val < 0.01, na.rm = TRUE), 0.10)
})

test_that(".eb_shrunk_dispersion raw_rho matches the FerruMod MoM formula in R", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  df <- .eb_windows_df()
  tmpdir <- withr::local_tempdir()
  con <- DBI::dbConnect(duckdb::duckdb(file.path(tmpdir, "t.mod.db")))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)
  DBI::dbWriteTable(con, "windows", df)

  in_dat <- dplyr::tbl(con, "windows") |>
    dplyr::select(sample_name, chrom, start, end, num_calls, mod_counts = m_counts) |>
    dplyr::mutate(exp_group = dplyr::if_else(grepl("^case", sample_name),
                                             "case", "control"))

  invisible(ModSeqR:::.eb_shrunk_dispersion(in_dat, c("chrom", "start", "end"),
                                            df_prior = 10))
  sql_rho <- DBI::dbGetQuery(con,
    "SELECT chrom, start, raw_rho FROM _eb_disp ORDER BY chrom, start")

  # Independent R recompute of the pooled method-of-moments rho.
  r_rho <- do.call(rbind, lapply(split(df, list(df$chrom, df$start), drop = TRUE),
    function(d) {
      k <- d$m_counts; n <- d$num_calls
      p_bar <- sum(k) / sum(n)
      s2    <- stats::var(k / n)
      n_bar <- mean(n)
      v_b   <- p_bar * (1 - p_bar) / n_bar
      raw   <- if (!is.na(s2) && s2 > v_b && (p_bar * (1 - p_bar) - v_b) > 0)
                 (s2 - v_b) / (p_bar * (1 - p_bar) - v_b) else 0
      data.frame(chrom = d$chrom[1], start = d$start[1], raw_rho = raw)
    }))
  m <- merge(sql_rho, r_rho, by = c("chrom", "start"), suffixes = c("_sql", "_r"))
  expect_equal(nrow(m), nrow(sql_rho))
  expect_lt(max(abs(m$raw_rho_sql - m$raw_rho_r)), 1e-8)
})

test_that("eb_beta_bin covariate path runs and returns valid p-values", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  df <- .eb_windows_df()
  meta <- data.frame(
    sample_name = c(paste0("case", 1:8), paste0("ctrl", 1:10)),
    age = c(rnorm(8, 75, 5), rnorm(10, 70, 5)))
  res <- .run_eb(df, covariates = "age", sample_meta = meta)
  expect_true(nrow(res) > 0)
  ok <- !is.na(res$p_val)
  expect_true(all(res$p_val[ok] >= 0 & res$p_val[ok] <= 1))
})

test_that("eb_beta_bin errors clearly when covariates given without sample_meta", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  expect_error(.run_eb(.eb_windows_df(), covariates = "age", sample_meta = NULL),
               "sample_meta")
})

test_that("eb_beta_bin parallel path == serial (skips if installed build lacks EB)", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  skip_if_not(requireNamespace("ModSeqR", quietly = TRUE))
  df <- .eb_windows_df()
  s <- .run_eb(df, ncores = 1L)
  p <- tryCatch(.run_eb(df, ncores = 2L),
                error = function(e) testthat::skip(paste("parallel unavailable:",
                                                         conditionMessage(e))))
  expect_equal(nrow(s), nrow(p))
  expect_equal(s$p_val,    p$p_val,    tolerance = 1e-6)
  expect_equal(s$p_adjust, p$p_adjust, tolerance = 1e-6)
})
