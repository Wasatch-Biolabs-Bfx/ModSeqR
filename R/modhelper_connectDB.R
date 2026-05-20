#' Connect to a Database (internal)
#'
#' Ensures that \code{mod_db} has an open DuckDB connection. If the connection
#' is already live it is reused; otherwise one is opened lazily. Bare character
#' strings are no longer accepted — callers must pass a \code{mod_db} object
#' created by \code{make_mod_db()} or \code{connect_mod_db()}.
#'
#' @param mod_db An object of class \code{mod_db}.
#'
#' @return The same \code{mod_db} object with a live connection in
#'   \code{.conn_env$con}.
#'
#' @importFrom DBI dbConnect dbIsValid
#' @importFrom duckdb duckdb
#'
#' @keywords internal

.modhelper_connectDB <- function(mod_db)
{
  if (!inherits(mod_db, "mod_db"))
    stop("Expected a 'mod_db' object. Use connect_mod_db() or make_mod_db() first.")

  env <- mod_db$.conn_env

  if (!is.null(env$con) && DBI::dbIsValid(env$con))
    return(mod_db)

  db_key <- normalizePath(mod_db$db_file, mustWork = FALSE)
  .check_not_connected(db_key)
  .open_persistent_connection(mod_db, db_key)
  mod_db
}
