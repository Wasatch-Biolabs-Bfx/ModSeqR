#' Get Table Names from a mod Database
#'
#' Returns a character vector of all table names in a \code{.mod.db} database
#' and prints them to the console.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#'
#' @return Invisibly returns a character vector of table names.
#'
#' @examples
#' \dontrun{
#' tables <- get_mod_tablelist("my_data.mod.db")
#'
#' # Or from an existing mod_db object
#' tables <- get_mod_tablelist(mod_db)
#' }
#'
#' @importFrom DBI dbListTables
#'
#' @export

get_mod_tablelist <- function(mod_db) {

  mod_db <- .modhelper_connectDB(mod_db)

  tables <- DBI::dbListTables(mod_db$con)

  cat("Tables in database:\n")
  if (length(tables) == 0) {
    cat("  (none)\n")
  } else {
    for (t in tables) cat("  ", t, "\n")
  }

  .modhelper_closeDB(mod_db)
  invisible(tables)
}
