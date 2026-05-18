#' Get Column Names from a DuckDB Table
#'
#' Returns a character vector of column names from a specified table
#' in a \code{.mod.db} database.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#' @param table_name Character. Name of the table whose column names are to be retrieved.
#'
#' @return Invisibly returns a character vector of column names. Also prints the
#'   names to the console.
#'
#' @examples
#' \dontrun{
#' cols <- get_mod_cols("my_data.mod.db", "mod_windows")
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbListFields dbListTables
#' @importFrom duckdb duckdb
#'
#' @export

get_mod_cols <- function(mod_db, table_name) {
  mod_db <- .modhelper_connectDB(mod_db)

  if (!(table_name %in% dbListTables(mod_db$con))) {
    .modhelper_closeDB(mod_db)
    stop(paste("Table", table_name, "does not exist in the database."))
  }

  col_names <- dbListFields(mod_db$con, table_name)

  cat(paste0("Columns in ", table_name, ":\n"))
  for (col in col_names) cat("  ", col, "\n")

  .modhelper_closeDB(mod_db)
  invisible(col_names)
}
