#' Remove a table from a database connection.
#'
#' This function checks if a table exists and, if so, removes it.
#' It provides messages about the outcome.
#'
#' @param mod_db A database object.
#' @param table_name A string specifying the name of the table to remove.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return).
#'   \code{last_result} is set to the name of the removed table (character).
#'
#' @importFrom DBI dbExistsTable dbRemoveTable
#' @importFrom glue glue
#'
#' @export
remove_mod_table <- function(mod_db, table_name) 
{
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)
  
  if (!is.character(table_name) || length(table_name) != 1 || nchar(trimws(table_name)) == 0) {
    stop("Error: 'table_name' must be a non-empty single string.")
  }
  
  if (dbExistsTable(mod_db$con, table_name)) {
    dbRemoveTable(mod_db$con, table_name)
    message(glue("Table '{table_name}' successfully removed from the database."))
    } 
  else {
    message(glue("Table '{table_name}' does not exist in the database. No action taken."))
  }
  
  mod_db$last_result <- table_name
  mod_db <- .modhelper_cleanup(mod_db)
  invisible(mod_db)
}