#' Close Database Connection
#'
#' This function updates the table list in the `mod_db` object and closes the database connection.
#'
#' @param mod_db An object containing information about the database, including the list of tables.
#'
#' @details
#' The function updates the `tables` attribute of the `mod_db` object with the current list of tables in the connected database 
#' before closing the connection.
#'
#' @return None. This function is called for its side effects (updating the object and closing the connection).
#'
#' @importFrom DBI dbDisconnect dbListTables dbIsValid
#'
#' @keywords internal

.modhelper_closeDB <- function(mod_db)
{
  if (is.character(mod_db$con) && mod_db$con == "none") {
    return(NULL)
  }
  
  if (dbIsValid(mod_db$con)) {
    dbDisconnect(mod_db$con, shutdown = TRUE)
    mod_db$con <- "none"
  }
  
  return(mod_db)
}