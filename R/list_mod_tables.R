#' List Tables in a mod Database
#'
#' Prints all table names in a \code{.mod.db} database and stores them as a
#' character vector in \code{mod_db$last_result}. Useful for quick inspection
#' or for capturing table names programmatically inside a pipe.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#'
#' @return Invisibly returns \code{mod_db} with \code{last_result} set to a
#'   character vector of table names. Retrieve it with
#'   \code{get_mod_result(mod_db)} or \code{mod_db$last_result}.
#'
#' @examples
#' \dontrun{
#' # Print tables and move on in a pipe
#' mod_db <- make_mod_db(ch3_files, "my_db") |>
#'   summarize_mod_windows() |>
#'   list_mod_tables()
#'
#' # Capture table names
#' mod_db <- list_mod_tables(mod_db)
#' tables <- get_mod_result(mod_db)   # character vector
#' }
#'
#' @importFrom DBI dbListTables
#'
#' @export

list_mod_tables <- function(mod_db) {

  mod_db <- .modhelper_connectDB(mod_db)

  tables <- DBI::dbListTables(mod_db$con)

  cat("Tables in database:\n")
  if (length(tables) == 0) {
    cat("  (none)\n")
  } else {
    for (t in tables) cat("  ", t, "\n")
  }

  mod_db$last_result <- tables
  mod_db <- .modhelper_closeDB(mod_db)
  invisible(mod_db)
}
