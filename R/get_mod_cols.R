#' Get Column Names from a DuckDB Table
#'
#' Returns a character vector of column names from a specified table
#' in a DuckDB database file.
#'
#' @param mod_db Path to the DuckDB database file (e.g., `"my_data.mod.db"`).
#' @param table_name Name of the table whose column names are to be retrieved.
#'
#' @return A character vector of column names from the specified table.
#'
#' @examples
#' \dontrun{
#' get_mod_cols("my_data.mod.db", "windows")
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbListFields
#' @importFrom duckdb duckdb
#' 
#' @export

get_mod_cols <- function(mod_db, table_name) {
  mod_db <- .modhelper_connectDB(mod_db)
  
  # Check if table exists
  if (!(table_name %in% dbListTables(mod_db$con))) {
    stop(paste("Table", table_name, "does not exist in the database."))
  }
  
  cat(paste0("Columns in ", table_name, " table:\n"))
  print(dbListFields(mod_db$con, table_name))
  
  mod_db <- .modhelper_closeDB(mod_db)
  invisible(mod_db)
}