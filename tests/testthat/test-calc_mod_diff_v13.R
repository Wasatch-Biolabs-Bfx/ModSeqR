# Tests for 1.3.0 additions: in-DuckDB closed-form tests (welch_t, prop_z,
# quasi_bin) and the parallel path for R-backed tests (parallel == serial).

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

# windows fixture: several samples/group, multiple chroms, real signal at some loci
.windows_df <- function(n_case = 6, n_ctrl = 8, n_loci = 30, seed = 11) {
  set.seed(seed)
  cases <- paste0("case", seq_len(n_case)); ctrls <- paste0("ctrl", seq_len(n_ctrl))
  do.call(rbind, lapply(seq_len(n_loci), function(i) {
    mu_c <- runif(1, 0.3, 0.7)
    mu_t <- if (i %% 3 == 0) min(max(mu_c + 0.3, 0.05), 0.95) else mu_c
    dc <- pmax(rpois(n_case, 6), 1); dt <- pmax(rpois(n_ctrl, 6), 1)
    data.frame(sample_name = c(cases, ctrls),
               chrom = paste0("chr", 1 + (i %% 3)),
               start = i * 100L, end = i * 100L + 1L, num_sites = 5L,
               num_calls = c(dc, dt),
               m_counts  = c(rbinom(n_case, dc, mu_c), rbinom(n_ctrl, dt, mu_t)),
               stringsAsFactors = FALSE)
  }))
}

.run_diff <- function(df, calc, ncores = 1L, cases = paste0("case", 1:6),
                      controls = paste0("ctrl", 1:8)) {
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
                                  calc_type = calc, min_samples = 2L, n_cores = ncores,
                                  overwrite = TRUE)
      out <- ModSeqR::get_mod_table(d, "d")
    })
  out[order(out$chrom, out$start), ]
}

test_that("in-DB welch_t / prop_z / quasi_bin produce valid p-values and columns", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  df <- .windows_df()
  for (calc in c("welch_t", "prop_z", "quasi_bin")) {
    res <- .run_diff(df, calc)
    expect_true(nrow(res) > 0)
    expect_true(all(c("meth_diff", "p_val", "p_adjust") %in% names(res)))
    ok <- !is.na(res$p_val)
    expect_true(all(res$p_val[ok] >= 0 & res$p_val[ok] <= 1))
    expect_true(all(res$p_adjust[!is.na(res$p_adjust)] >= 0 &
                    res$p_adjust[!is.na(res$p_adjust)] <= 1))
  }
})

test_that("quasi_bin reports overdispersion >= 1", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  res <- .run_diff(.windows_df(), "quasi_bin")
  expect_true("overdispersion" %in% names(res))
  expect_true(all(res$overdispersion[!is.na(res$overdispersion)] >= 1 - 1e-9))
})

test_that("prop_z matches R chisq.test on pooled counts", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  df <- .windows_df()
  res <- .run_diff(df, "prop_z")
  ref <- vapply(seq_len(nrow(res)), function(i) {
    d <- df[df$chrom == res$chrom[i] & df$start == res$start[i], ]
    xc <- sum(d$m_counts[grepl("^case", d$sample_name)])
    nc <- sum(d$num_calls[grepl("^case", d$sample_name)])
    xt <- sum(d$m_counts[grepl("^ctrl", d$sample_name)])
    nt <- sum(d$num_calls[grepl("^ctrl", d$sample_name)])
    suppressWarnings(chisq.test(matrix(c(xc, nc - xc, xt, nt - xt), 2),
                                correct = FALSE)$p.value)
  }, numeric(1))
  ok <- !is.na(res$p_val) & !is.na(ref)
  expect_lt(max(abs(res$p_val[ok] - ref[ok])), 1e-3)
})

test_that("parallel path == serial for beta_bin and fast_fisher", {
  skip_if_not_installed("duckdb"); skip_if_not_installed("withr")
  # PSOCK workers load the *installed* ModSeqR; skip if it isn't the build under test.
  skip_if_not(requireNamespace("ModSeqR", quietly = TRUE))
  df <- .windows_df()
  for (calc in c("beta_bin", "fast_fisher")) {
    s <- .run_diff(df, calc, ncores = 1L)
    p <- tryCatch(.run_diff(df, calc, ncores = 2L),
                  error = function(e) testthat::skip(paste("parallel unavailable:",
                                                           conditionMessage(e))))
    expect_equal(nrow(s), nrow(p))
    expect_equal(s$p_val,    p$p_val,    tolerance = 1e-9)
    expect_equal(s$p_adjust, p$p_adjust, tolerance = 1e-9)
  }
})
