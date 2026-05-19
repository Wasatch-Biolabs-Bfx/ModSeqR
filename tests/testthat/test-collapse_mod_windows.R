# Tests for collapse_mod_windows()

.make_mod_db_obj <- function(con, dbfile) {
  obj <- list(db_file = dbfile, current_table = NULL, con = con, last_result = NULL)
  class(obj) <- "mod_db"
  obj
}

# A minimal mod_diff_windows table: two adjacent significant windows that
# should merge, plus one that fails the filters.
.diff_windows_df <- function() {
  data.frame(
    chrom      = c("chr1", "chr1", "chr1"),
    start      = c(1000L, 1500L, 5000L),
    end        = c(1499L, 1999L, 5499L),
    p_adjust   = c(0.01,  0.02,  0.9),     # third is not significant
    meth_diff  = c(0.7,   0.65,  -0.8)     # first two same direction
  )
}

test_that("collapse_mod_windows merges adjacent significant windows", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "mod_diff_windows", .diff_windows_df())
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    collapse_mod_windows(mod_db_obj,
                         input_table  = "mod_diff_windows",
                         output_table = "collapsed_windows",
                         max_distance = 1000,
                         sig_cutoff   = 0.05,
                         min_diff     = 0.5)
  )

  result <- dplyr::tbl(con, "collapsed_windows") |> dplyr::collect()

  # Third window is below sig_cutoff — should be excluded
  # First two windows are within max_distance and same direction → one region
  expect_equal(nrow(result), 1L)
  expect_equal(result$chrom, "chr1")
  expect_equal(result$start, 1000L)
  expect_equal(result$end,   1999L)
  expect_equal(result$num_windows, 2L)
})

test_that("collapse_mod_windows separates windows of opposite meth_diff direction", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(
    chrom     = "chr1",
    start     = c(1000L, 1500L),
    end       = c(1499L, 1999L),
    p_adjust  = c(0.01, 0.01),
    meth_diff = c(0.7, -0.7)   # opposite directions → separate regions
  )
  DBI::dbWriteTable(con, "mod_diff_windows", df)
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    collapse_mod_windows(mod_db_obj,
                         input_table  = "mod_diff_windows",
                         output_table = "collapsed_windows",
                         max_distance = 1000,
                         sig_cutoff   = 0.05,
                         min_diff     = 0.5)
  )

  result <- dplyr::tbl(con, "collapsed_windows") |> dplyr::collect()
  expect_equal(nrow(result), 2L)
})

test_that("collapse_mod_windows respects min_diff filter", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(
    chrom     = "chr1",
    start     = c(100L, 200L),
    end       = c(199L, 299L),
    p_adjust  = c(0.01, 0.01),
    meth_diff = c(0.3, 0.8)   # first below min_diff = 0.5
  )
  DBI::dbWriteTable(con, "mod_diff_windows", df)
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    collapse_mod_windows(mod_db_obj,
                         input_table  = "mod_diff_windows",
                         output_table = "collapsed_windows",
                         min_diff = 0.5)
  )

  result <- dplyr::tbl(con, "collapsed_windows") |> dplyr::collect()
  expect_equal(nrow(result), 1L)
  expect_equal(result$start, 200L)
})

test_that("collapse_mod_windows errors when input table is missing", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    expect_error(
      collapse_mod_windows(mod_db_obj),
      regexp = "not found|does not exist", fixed = FALSE
    )
  )
})

test_that("collapse_mod_windows sets current_table and last_result", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  DBI::dbWriteTable(con, "mod_diff_windows", .diff_windows_df())
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  res <- testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    collapse_mod_windows(mod_db_obj,
                         input_table  = "mod_diff_windows",
                         output_table = "collapsed_windows",
                         sig_cutoff   = 0.05,
                         min_diff     = 0.5)
  )

  expect_equal(res$current_table, "collapsed_windows")
  expect_false(is.null(res$last_result))
})
