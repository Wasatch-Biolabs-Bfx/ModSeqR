# Tests for filter_mod_table()

.make_mod_db_obj <- function(con, dbfile) {
  obj <- list(db_file = dbfile, current_table = NULL, con = con, last_result = NULL)
  class(obj) <- "mod_db"
  obj
}

test_that("filter_mod_table keeps only rows matching the filter expression", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(
    chrom  = c("chr1","chr1","chr2","chr2"),
    start  = c(100L, 200L, 100L, 200L),
    score  = c(0.8, 0.3, 0.9, 0.1)
  )
  DBI::dbWriteTable(con, "source_table", df)
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    filter_mod_table(mod_db_obj,
                     input_table  = "source_table",
                     output_table = "filtered_table",
                     score > 0.5)
  )

  result <- dplyr::tbl(con, "filtered_table") |> dplyr::collect()

  expect_equal(nrow(result), 2L)
  expect_true(all(result$score > 0.5))
})

test_that("filter_mod_table can overwrite the input table in-place", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(chrom = "chr1",
                   start = c(1L, 2L, 3L, 4L, 5L),
                   score = c(0.1, 0.9, 0.4, 0.8, 0.2))
  DBI::dbWriteTable(con, "windows", df)
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    filter_mod_table(mod_db_obj,
                     input_table  = "windows",
                     output_table = "windows",
                     score >= 0.8)
  )

  result <- dplyr::tbl(con, "windows") |> dplyr::collect()
  expect_equal(nrow(result), 2L)
  expect_true(all(result$score >= 0.8))
})

test_that("filter_mod_table errors when output_table is missing", {
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
      filter_mod_table(mod_db_obj, input_table = "t"),
      regexp = "output_table"
    )
  )
})

test_that("filter_mod_table sets last_result to row counts per sample when sample_name present", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("dplyr")

  tmpdir <- withr::local_tempdir()
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  on.exit(DBI::dbDisconnect(con, shutdown = TRUE), add = TRUE)

  df <- data.frame(
    sample_name = c("s1","s1","s2","s2"),
    chrom       = "chr1",
    start       = c(1L, 2L, 1L, 2L),
    score       = c(0.9, 0.2, 0.8, 0.1)
  )
  DBI::dbWriteTable(con, "src", df)
  mod_db_obj <- .make_mod_db_obj(con, dbfile)

  res <- testthat::with_mocked_bindings(.package = "ModSeqR",
    .modhelper_connectDB = function(x) x,
    .modhelper_cleanup   = function(x) x,
    filter_mod_table(mod_db_obj, input_table = "src",
                     output_table = "out", score > 0.5)
  )

  expect_true(is.data.frame(res$last_result))
  expect_true("sample_name" %in% names(res$last_result))
  expect_true("n" %in% names(res$last_result))
  expect_equal(sum(res$last_result$n), 2L)
})
