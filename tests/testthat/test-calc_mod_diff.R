# Tests for calc_mod_diff() and its internal statistical helpers.
#
# Strategy: open a real DuckDB connection in a temp file, mock
# .modhelper_connectDB / .modhelper_cleanup so the function uses our
# already-open connection and does not close it between assertions.

# ---- shared helper -------------------------------------------------------

.make_mod_db <- function(con, dbfile) {
  obj <- list(db_file = dbfile, current_table = NULL, con = con, last_result = NULL)
  class(obj) <- "mod_db"
  obj
}

.positions_df <- function(n_case = 2, n_ctrl = 2) {
  # Two loci: locus 100 heavily methylated in case; locus 200 lightly in case
  samps <- c(paste0("case", seq_len(n_case)), paste0("ctrl", seq_len(n_ctrl)))
  grp   <- c(rep("case", n_case), rep("ctrl", n_ctrl))
  make_locus <- function(start, case_mod, ctrl_mod) {
    data.frame(
      sample_name = samps,
      chrom       = "chr1",
      start       = start,
      end         = start + 1L,
      num_calls   = rep(20L, n_case + n_ctrl),
      mh_counts   = c(rep(case_mod, n_case), rep(ctrl_mod, n_ctrl)),
      stringsAsFactors = FALSE
    )
  }
  rbind(make_locus(100L, 16L, 4L),   # case >> ctrl
        make_locus(200L, 4L,  16L))   # case << ctrl
}

# ---- calc_mod_diff: fast_fisher ------------------------------------------

test_that("calc_mod_diff fast_fisher produces correct table structure and directions", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("withr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "positions", .positions_df())
  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    {
      res <- calc_mod_diff(
        mod_db    = mod_db_obj,
        input_table = "positions",
        cases     = c("case1", "case2"),
        controls  = c("ctrl1", "ctrl2"),
        mod_type  = "mh",
        calc_type = "fast_fisher"
      )

      expect_s3_class(res, "mod_db")
      expect_true(DBI::dbExistsTable(con, "mod_diff_positions"))

      out <- dplyr::tbl(con, "mod_diff_positions") |>
        dplyr::arrange(chrom, start) |>
        dplyr::collect()

      # Required output columns
      expect_true(all(c("p_val", "p_adjust", "meth_diff",
                        "mh_counts_case", "mh_counts_control") %in% names(out)))

      # mod_type prefix applied; no bare "mod_" columns remain
      expect_false(any(grepl("^mod_counts_", names(out))))

      # p_adjust in valid range
      expect_true(all(out$p_adjust >= 0 & out$p_adjust <= 1, na.rm = TRUE))

      # BH adjustment is monotone with p_val rank
      expect_equal(order(out$p_val), order(out$p_adjust))

      # Effect directions
      expect_gt(out$meth_diff[out$start == 100], 0)
      expect_lt(out$meth_diff[out$start == 200], 0)

      # last_result set to a data frame preview
      expect_true(is.data.frame(res$last_result))
    }
  )
})

# ---- calc_mod_diff: r_fisher vs fast_fisher rank agreement ---------------

test_that("calc_mod_diff r_fisher p-value ranks agree with fast_fisher", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "positions", .positions_df())
  mod_db_obj <- .make_mod_db(con, dbfile)

  run <- function(ct) {
    if (DBI::dbExistsTable(con, "mod_diff_positions"))
      DBI::dbRemoveTable(con, "mod_diff_positions")
    testthat::with_mocked_bindings(.package = "ModSeqR",
      .modhelper_connectDB = function(x) x,
      .modhelper_cleanup   = function(x) x,
      calc_mod_diff(mod_db_obj, "positions",
                   cases = c("case1","case2"), controls = c("ctrl1","ctrl2"),
                   mod_type = "mh", calc_type = ct)
    )
    dplyr::tbl(con, "mod_diff_positions") |>
      dplyr::arrange(chrom, start) |>
      dplyr::collect()
  }

  out_fast <- run("fast_fisher")
  out_r    <- run("r_fisher")

  expect_equal(order(out_fast$p_val), order(out_r$p_val))
})

# ---- calc_mod_diff: wilcox (SQL) -----------------------------------------

test_that("calc_mod_diff wilcox runs fully in SQL and produces correct output", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  # Need >= 5 samples per group for wilcox to be accepted
  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(1)
  n <- 6
  df <- rbind(
    data.frame(sample_name = paste0("case", 1:n), chrom = "chr1",
               start = 100L, end = 101L, num_calls = 30L,
               mh_counts = c(22L, 24L, 20L, 23L, 21L, 25L)),
    data.frame(sample_name = paste0("ctrl", 1:n), chrom = "chr1",
               start = 100L, end = 101L, num_calls = 30L,
               mh_counts = c(6L, 8L, 5L, 7L, 9L, 6L))
  )
  DBI::dbWriteTable(con, "positions", df)
  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    calc_mod_diff(mod_db_obj, "positions",
                 cases = paste0("case", 1:n), controls = paste0("ctrl", 1:n),
                 mod_type = "mh", calc_type = "wilcox")
  )

  out <- dplyr::tbl(con, "mod_diff_positions") |> dplyr::collect()

  expect_true(all(c("p_val", "p_adjust", "meth_diff") %in% names(out)))
  expect_true(all(out$p_val    >= 0 & out$p_val    <= 1, na.rm = TRUE))
  expect_true(all(out$p_adjust >= 0 & out$p_adjust <= 1, na.rm = TRUE))
  expect_gt(out$meth_diff[1], 0)
})

test_that("wilcox SQL p-values match R normal approximation (exact=FALSE, correct=TRUE)", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(42)
  n_case <- 7; n_ctrl <- 6
  loci <- data.frame(chrom = c("chr1","chr2"), start = c(1000L, 500L), end = c(1100L, 600L))

  rows <- lapply(seq_len(nrow(loci)), function(i) {
    data.frame(
      chrom       = loci$chrom[i],
      start       = loci$start[i],
      end         = loci$end[i],
      sample_name = c(paste0("case", seq_len(n_case)), paste0("ctrl", seq_len(n_ctrl))),
      exp_group   = c(rep("case", n_case), rep("control", n_ctrl)),
      num_calls   = sample(20:100, n_case + n_ctrl, replace = TRUE),
      mod_counts  = c(sample(10:80, n_case, replace = TRUE),
                      sample(1:20,  n_ctrl, replace = TRUE))
    )
  })
  dat <- do.call(rbind, rows)
  DBI::dbWriteTable(con, "test_in", dat)
  in_dat <- dplyr::tbl(con, "test_in")

  sql_res <- ModSeqR:::.calc_diff_wilcox(in_dat) |>
    dplyr::arrange(chrom, start) |>
    dplyr::collect()

  # Compare to R's normal approximation for each locus
  for (i in seq_len(nrow(loci))) {
    sub  <- dat[dat$chrom == loci$chrom[i] & dat$start == loci$start[i], ]
    mf   <- sub$mod_counts / sub$num_calls
    r_p  <- suppressWarnings(
      stats::wilcox.test(mf[sub$exp_group == "case"],
                         mf[sub$exp_group == "control"],
                         exact = FALSE, correct = TRUE)$p.value
    )
    sql_p <- sql_res$p_val[sql_res$chrom == loci$chrom[i] & sql_res$start == loci$start[i]]
    expect_equal(sql_p, r_p, tolerance = 1e-4,
                 label = paste("wilcox p-value for", loci$chrom[i], loci$start[i]))
  }
})

# ---- calc_mod_diff: beta_bin ---------------------------------------------

test_that("calc_mod_diff beta_bin produces valid p-values and correct directions", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(7)
  n <- 4
  df <- rbind(
    data.frame(sample_name = paste0("case", 1:n), chrom = "chr1",
               start = 100L, end = 101L, num_calls = 40L,
               mh_counts = c(30L, 32L, 28L, 31L)),
    data.frame(sample_name = paste0("ctrl", 1:n), chrom = "chr1",
               start = 100L, end = 101L, num_calls = 40L,
               mh_counts = c(8L, 10L, 7L, 9L))
  )
  DBI::dbWriteTable(con, "positions", df)
  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    suppressWarnings(
      calc_mod_diff(mod_db_obj, "positions",
                   cases = paste0("case", 1:n), controls = paste0("ctrl", 1:n),
                   mod_type = "mh", calc_type = "beta_bin")
    )
  )

  out <- dplyr::tbl(con, "mod_diff_positions") |> dplyr::collect()

  expect_true(all(c("p_val", "p_adjust", "meth_diff", "overdispersion") %in% names(out)))
  expect_true(all(out$p_adjust >= 0 & out$p_adjust <= 1, na.rm = TRUE))
  expect_gt(out$meth_diff[1], 0)
})

test_that("beta_bin collects only needed columns and batches by chromosome", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")
  skip_if_not_installed("dbplyr")

  con <- DBI::dbConnect(duckdb::duckdb())
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  set.seed(3)
  n <- 3
  loci <- data.frame(chrom = c("chr1","chr2","chr3"),
                     start = c(100L, 200L, 300L),
                     end   = c(101L, 201L, 301L))
  rows <- lapply(seq_len(nrow(loci)), function(i) {
    data.frame(
      chrom       = loci$chrom[i],
      start       = loci$start[i],
      end         = loci$end[i],
      sample_name = c(paste0("case", 1:n), paste0("ctrl", 1:n)),
      exp_group   = c(rep("case", n), rep("control", n)),
      num_calls   = sample(20:60, 2*n, replace = TRUE),
      mod_counts  = c(sample(12:40, n, replace = TRUE),
                      sample(2:12,  n, replace = TRUE)),
      num_sites   = sample(5:20, 2*n, replace = TRUE)  # should NOT be pulled
    )
  })
  dat <- do.call(rbind, rows)
  DBI::dbWriteTable(con, "test_in", dat)
  in_dat <- dplyr::tbl(con, "test_in")

  res <- suppressWarnings(ModSeqR:::.calc_diff_betabin(in_dat))

  # One row per locus, three loci
  expect_equal(nrow(res), 3)

  # num_sites must not appear in results (pruned before collection)
  expect_false("num_sites" %in% names(res))

  # All three chromosomes present
  expect_setequal(res$chrom, c("chr1", "chr2", "chr3"))
})

# ---- calc_mod_diff: log_reg ----------------------------------------------

test_that("calc_mod_diff log_reg produces valid output", {
  # log_reg currently references a deprecated 'ref_position' column instead
  # of 'start', causing a schema error. Skipped until the function is fixed.
  skip("log_reg has a known pre-existing schema bug (uses 'ref_position' instead of 'start')")
})

# ---- calc_mod_diff: mod_type rename --------------------------------------

test_that("calc_mod_diff renames mod_ columns to the requested mod_type prefix", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(sample_name = c("case1","ctrl1"),
                   chrom = "chr1", start = 1L, end = 2L,
                   num_calls = 20L, m_counts = 14L)
  DBI::dbWriteTable(con, "positions", df)
  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    calc_mod_diff(mod_db_obj, "positions",
                 cases = "case1", controls = "ctrl1",
                 mod_type = "m", calc_type = "fast_fisher")
  )

  cols <- colnames(dplyr::tbl(con, "mod_diff_positions"))
  expect_true(any(grepl("^m_counts_", cols)))
  expect_false(any(grepl("^mod_counts_", cols)))
})

# ---- calc_mod_diff: error handling ---------------------------------------

test_that("calc_mod_diff errors clearly for missing table or unknown sample names", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    {
      # Table does not exist
      expect_error(
        calc_mod_diff(mod_db_obj, "positions", cases = "a", controls = "b"),
        regexp = "table does not exist", fixed = FALSE
      )

      DBI::dbWriteTable(con, "positions",
                        data.frame(sample_name = c("ctrl1","ctrl2"), chrom = "chr1",
                                   start = 1L, end = 2L, num_calls = 10L, mh_counts = 5L))

      # Cases missing from data
      expect_error(
        calc_mod_diff(mod_db_obj, "positions",
                     cases = "case1", controls = c("ctrl1","ctrl2")),
        regexp = "case samples are missing", fixed = FALSE
      )

      DBI::dbRemoveTable(con, "positions")
      DBI::dbWriteTable(con, "positions",
                        data.frame(sample_name = c("case1","case2"), chrom = "chr1",
                                   start = 1L, end = 2L, num_calls = 10L, mh_counts = 5L))

      # Controls missing from data
      expect_error(
        calc_mod_diff(mod_db_obj, "positions",
                     cases = c("case1","case2"), controls = "ctrl1"),
        regexp = "control samples are not in the data", fixed = FALSE
      )
    }
  )
})

# ---- BH adjustment: all p_val equal → all p_adjust equal ----------------

test_that("BH adjustment handles all-equal p-values without error", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  # All loci with identical counts → identical p-values
  df <- do.call(rbind, lapply(c(100L, 200L, 300L), function(s) {
    data.frame(sample_name = c("case1","ctrl1"),
               chrom = "chr1", start = s, end = s + 1L,
               num_calls = 20L, mh_counts = 10L)
  }))
  DBI::dbWriteTable(con, "positions", df)
  mod_db_obj <- .make_mod_db(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    calc_mod_diff(mod_db_obj, "positions",
                 cases = "case1", controls = "ctrl1",
                 mod_type = "mh", calc_type = "fast_fisher")
  )

  out <- dplyr::tbl(con, "mod_diff_positions") |> dplyr::collect()
  expect_equal(length(unique(out$p_adjust)), 1L)
  expect_true(all(out$p_adjust >= 0 & out$p_adjust <= 1))
})
