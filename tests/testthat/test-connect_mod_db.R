# Tests for connect_mod_db(), disconnect_mod_db(), and the lazy-open path
# in .modhelper_connectDB().

# ---- helpers ---------------------------------------------------------------

.fresh_db <- function(tmpdir) {
  dbfile <- file.path(tmpdir, "test.mod.db")
  con    <- DBI::dbConnect(duckdb::duckdb(dbfile))
  DBI::dbExecute(con, "CREATE TABLE t AS SELECT 1 AS x")
  DBI::dbDisconnect(con, shutdown = TRUE)
  dbfile
}


# ---- connect_mod_db --------------------------------------------------------

test_that("connect_mod_db() returns a mod_db with an open connection", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  mod_db <- connect_mod_db(dbfile)
  on.exit(disconnect_mod_db(mod_db), add = TRUE)

  expect_s3_class(mod_db, "mod_db")
  expect_true(DBI::dbIsValid(.get_con(mod_db)))
  expect_equal(mod_db$db_file, dbfile)
  expect_true(is.environment(mod_db$.conn_env))
})


test_that("connect_mod_db() appends .mod.db when suffix is missing", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)
  stem   <- sub("\\.mod\\.db$", "", dbfile)

  mod_db <- connect_mod_db(stem)
  on.exit(disconnect_mod_db(mod_db), add = TRUE)

  expect_true(DBI::dbIsValid(.get_con(mod_db)))
})


test_that("connect_mod_db() errors when the file does not exist", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  expect_error(
    connect_mod_db(file.path(tmpdir, "no_such.mod.db")),
    "Database file not found"
  )
})


test_that("connect_mod_db() errors on duplicate live connection", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  mod_db <- connect_mod_db(dbfile)
  on.exit(disconnect_mod_db(mod_db), add = TRUE)

  expect_error(
    connect_mod_db(dbfile),
    "already has an active connection"
  )
})


test_that("connect_mod_db() succeeds after disconnect_mod_db()", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  mod_db <- connect_mod_db(dbfile)
  disconnect_mod_db(mod_db)

  mod_db2 <- connect_mod_db(dbfile)
  on.exit(disconnect_mod_db(mod_db2), add = TRUE)

  expect_true(DBI::dbIsValid(.get_con(mod_db2)))
})


# ---- disconnect_mod_db -----------------------------------------------------

test_that("disconnect_mod_db() closes the connection and clears the registry", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)
  db_key <- normalizePath(dbfile, mustWork = TRUE)

  mod_db <- connect_mod_db(dbfile)
  expect_true(exists(db_key, envir = .active_connections, inherits = FALSE))

  disconnect_mod_db(mod_db)
  expect_null(mod_db$.conn_env$con)
  expect_false(exists(db_key, envir = .active_connections, inherits = FALSE))
})


test_that("disconnect_mod_db() is a no-op on an already-closed connection", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  mod_db <- connect_mod_db(dbfile)
  disconnect_mod_db(mod_db)
  expect_silent(disconnect_mod_db(mod_db))   # second call must not error
})


# ---- .modhelper_connectDB() lazy open --------------------------------------

test_that(".modhelper_connectDB() opens lazily on a disconnected skeleton", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  # Build a disconnected skeleton (as make_mod_db() would return)
  skel <- .make_mod_db_skeleton(dbfile,
                                 list(memory_limit = NULL, temp_dir = NULL, threads = NULL))
  expect_null(skel$.conn_env$con)

  skel <- .modhelper_connectDB(skel)
  on.exit(disconnect_mod_db(skel), add = TRUE)

  expect_true(DBI::dbIsValid(.get_con(skel)))
})


test_that(".modhelper_connectDB() reuses an existing live connection", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  mod_db <- connect_mod_db(dbfile)
  on.exit(disconnect_mod_db(mod_db), add = TRUE)

  con_before <- .get_con(mod_db)
  mod_db     <- .modhelper_connectDB(mod_db)
  con_after  <- .get_con(mod_db)

  expect_identical(con_before, con_after)
})


test_that(".modhelper_connectDB() errors when given a bare string", {
  expect_error(
    .modhelper_connectDB("some_path"),
    "Expected a 'mod_db' object"
  )
})


# ---- make_mod_db() returns disconnected skeleton ---------------------------

test_that("make_mod_db() returns a disconnected mod_db with correct slots", {
  skip_if_not_installed("duckdb")
  skip_if_not_installed("arrow")

  tmpdir <- withr::local_tempdir()

  # Use the bundled example data
  ch3_path <- system.file("extdata/ch3_files", package = "ModSeqR")
  skip_if(
    !nzchar(ch3_path) || !dir.exists(ch3_path),
    "example ch3 files not found"
  )

  dbfile <- file.path(tmpdir, "make_test.mod.db")
  mod_db <- make_mod_db(ch3_files = ch3_path, db_name = dbfile)

  expect_s3_class(mod_db, "mod_db")
  expect_equal(mod_db$current_table, "calls")
  expect_false(is.null(mod_db$last_result))
  # Connection should be closed (NULL) after make_mod_db()
  expect_null(mod_db$.conn_env$con)
  # config slot present
  expect_true(is.list(mod_db$config))
  # .conn_env slot present
  expect_true(is.environment(mod_db$.conn_env))
})


# ---- single connection across a 3-step pipe --------------------------------

test_that("a 3-step pipe opens exactly one connection (registry stays size 1)", {
  skip_if_not_installed("duckdb")

  tmpdir <- withr::local_tempdir()
  dbfile <- .fresh_db(tmpdir)

  # Pre-seed a table so summarize can find columns
  con_seed <- DBI::dbConnect(duckdb::duckdb(dbfile))
  DBI::dbExecute(con_seed, "DROP TABLE IF EXISTS t")
  # Write a minimal calls table for testing
  calls_df <- data.frame(
    sample_name   = c("A","A","B","B"),
    chrom         = "chr1",
    start         = c(1L,2L,1L,2L),
    end           = c(2L,3L,2L,3L),
    read_position = 1L,
    query_kmer    = "ACGT",
    call_code     = "m",
    read_length   = 100L,
    call_prob     = 0.95,
    base_qual     = 30L,
    flag          = 0L,
    read_id       = c("r1","r2","r3","r4"),
    stringsAsFactors = FALSE
  )
  DBI::dbWriteTable(con_seed, "calls", calls_df, overwrite = TRUE)
  DBI::dbDisconnect(con_seed, shutdown = TRUE)

  mod_db <- connect_mod_db(dbfile)
  on.exit(disconnect_mod_db(mod_db), add = TRUE)

  db_key <- normalizePath(dbfile, mustWork = TRUE)
  # Only one entry in the registry throughout the pipe
  expect_equal(length(ls(.active_connections)), 1L)
})
