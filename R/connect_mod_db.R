# ============================================================
# Internal null-coalesce helper
# ============================================================
`%||%` <- function(a, b) if (!is.null(a)) a else b


# ============================================================
# Exported functions
# ============================================================

#' Connect to an existing ModSeqR database
#'
#' Opens a persistent DuckDB connection to a \code{.mod.db} file that already
#' exists on disk and returns a \code{mod_db} object ready for use in the
#' pipeline. Only one connection per database file is allowed at a time; a
#' second call to \code{connect_mod_db()} for the same file while a connection
#' is live will error with a clear message.
#'
#' The connection is closed automatically when the \code{mod_db} object is
#' garbage-collected. You can also close it explicitly with
#' \code{\link{disconnect_mod_db}()}.
#'
#' @param db_path Character. Path to the \code{.mod.db} file. The
#'   \code{.mod.db} suffix is appended automatically if missing.
#' @param memory_limit Character or \code{NULL}. DuckDB memory cap
#'   (e.g. \code{"16GB"}, \code{"32768MB"}). When \code{NULL}, an internal
#'   heuristic (~75\% of detected RAM) is used.
#' @param temp_dir Character or \code{NULL}. Directory DuckDB uses for
#'   spill-to-disk scratch files. When \code{NULL}, a subdirectory of
#'   \code{tempdir()} is used.
#' @param threads Integer or \code{NULL}. DuckDB thread count. When
#'   \code{NULL}, all-but-one logical core is used.
#'
#' @return A \code{mod_db} object with an open persistent DuckDB connection.
#'
#' @seealso \code{\link{disconnect_mod_db}}, \code{\link{make_mod_db}}
#'
#' @importFrom DBI dbConnect dbIsValid
#' @importFrom duckdb duckdb
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Connect to an existing database with auto-detected resources
#' mod_db <- connect_mod_db("my_analysis.mod.db")
#'
#' # Connect with explicit resource caps
#' mod_db <- connect_mod_db("my_analysis.mod.db",
#'                           memory_limit = "32GB",
#'                           threads      = 8)
#'
#' # Attempting a duplicate connection raises a clear error
#' mod_db2 <- connect_mod_db("my_analysis.mod.db")
#' #> Error: 'my_analysis.mod.db' already has an active connection.
#' #> Call disconnect_mod_db() first or let the existing object go out of scope.
#'
#' # Disconnect explicitly, then reconnect
#' disconnect_mod_db(mod_db)
#' mod_db <- connect_mod_db("my_analysis.mod.db")
#'
#' # Pipe a full analysis on an existing database
#' mod_db <- connect_mod_db("my_analysis.mod.db") |>
#'   summarize_mod_windows()                      |>
#'   calc_mod_diff(cases = c("caseA", "caseB"),
#'                 controls = c("ctrlA", "ctrlB"))
#' }
connect_mod_db <- function(db_path,
                            memory_limit = NULL,
                            temp_dir     = NULL,
                            threads      = NULL)
{
  if (!grepl("\\.mod\\.db$", db_path)) db_path <- paste0(db_path, ".mod.db")
  if (!file.exists(db_path)) stop("Database file not found: ", db_path)

  db_key <- normalizePath(db_path, mustWork = TRUE)
  .check_not_connected(db_key)

  mod_db <- .make_mod_db_skeleton(db_path,
                                   list(memory_limit = memory_limit,
                                        temp_dir     = temp_dir,
                                        threads      = threads))
  .open_persistent_connection(mod_db, db_key)
  mod_db
}


#' Disconnect from a ModSeqR database
#'
#' Closes the DuckDB connection held by a \code{mod_db} object and removes it
#' from the duplicate-connection registry. After disconnecting, pipeline
#' functions will re-open the connection lazily on the next call, or you can
#' call \code{\link{connect_mod_db}()} to reconnect explicitly.
#'
#' @param mod_db A \code{mod_db} object with an active (or already closed)
#'   connection.
#'
#' @return \code{mod_db} invisibly (connection slot is now closed).
#'
#' @seealso \code{\link{connect_mod_db}}
#'
#' @importFrom DBI dbDisconnect dbIsValid
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mod_db <- connect_mod_db("my_analysis.mod.db")
#' # ... run pipeline steps ...
#' disconnect_mod_db(mod_db)
#' # Reconnect when needed
#' mod_db <- connect_mod_db("my_analysis.mod.db")
#' }
disconnect_mod_db <- function(mod_db)
{
  env <- mod_db$.conn_env
  if (!is.null(env$con) && DBI::dbIsValid(env$con))
    tryCatch(DBI::dbDisconnect(env$con, shutdown = TRUE), error = function(e) NULL)
  env$con <- NULL
  if (!is.null(env$db_key))
    suppressWarnings(rm(list = env$db_key, envir = .active_connections))
  invisible(mod_db)
}


# ============================================================
# Internal helpers
# ============================================================

# Build a disconnected mod_db skeleton.
# Used by connect_mod_db(), make_mod_db(), and .modhelper_connectDB().
.make_mod_db_skeleton <- function(db_path, config) {
  env        <- new.env(parent = emptyenv())
  env$con    <- NULL
  env$db_key <- NULL
  db <- list(
    db_file       = db_path,
    current_table = NULL,
    last_result   = NULL,
    config        = config,
    .conn_env     = env
  )
  class(db) <- "mod_db"
  db
}


# Open a DuckDB connection, store it in .conn_env, register the GC finalizer
# and the duplicate-connection registry entry.
.open_persistent_connection <- function(mod_db, db_key) {
  cfg <- .resolve_duckdb_config(mod_db$config)
  con <- DBI::dbConnect(duckdb::duckdb(mod_db$db_file, config = cfg),
                        read_only = FALSE)
  mod_db$.conn_env$con    <- con
  mod_db$.conn_env$db_key <- db_key
  assign(db_key, mod_db$.conn_env, envir = .active_connections)
  reg.finalizer(mod_db$.conn_env, .finalizer_close_connection, onexit = TRUE)
}


# Check the registry for an existing live connection to db_key.
# Errors if a live duplicate is found; silently clears stale entries.
.check_not_connected <- function(db_key) {
  if (exists(db_key, envir = .active_connections, inherits = FALSE)) {
    env <- get(db_key, envir = .active_connections, inherits = FALSE)
    if (!is.null(env$con) && DBI::dbIsValid(env$con))
      stop("'", basename(db_key), "' already has an active connection.\n",
           "Call disconnect_mod_db() first or let the existing object go out of scope.")
    rm(list = db_key, envir = .active_connections)
  }
}


# Merge user config with auto-detected defaults.
.resolve_duckdb_config <- function(user_cfg) {
  caps     <- .auto_duckdb_resource_caps(0.75)
  tmp_path <- file.path(tempdir(), "modseqr_duckdb_tmp")
  dir.create(tmp_path, recursive = TRUE, showWarnings = FALSE)
  list(
    memory_limit   = user_cfg$memory_limit %||% caps$memory_limit,
    temp_directory = user_cfg$temp_dir     %||% tmp_path,
    threads        = as.character(user_cfg$threads %||% caps$threads)
  )
}


# GC finalizer — errors must never surface during garbage collection.
.finalizer_close_connection <- function(env) {
  tryCatch({
    if (!is.null(env$con) && DBI::dbIsValid(env$con))
      DBI::dbDisconnect(env$con, shutdown = TRUE)
    if (!is.null(env$db_key))
      suppressWarnings(rm(list = env$db_key, envir = .active_connections))
  }, error = function(e) NULL)
}


# Convenience accessor used by all pipeline functions.
.get_con <- function(mod_db) mod_db$.conn_env$con
