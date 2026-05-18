#' Print a mod_db Object
#'
#' S3 print method for objects of class \code{"mod_db"}. Displays the database
#' file path, the current active table, the connection status, and — if a
#' result has been stored — the class and dimensions of \code{last_result}.
#'
#' @param mod_db An object of class \code{"mod_db"}.
#'
#' @details
#' Output example:
#' \preformatted{
#' <mod_db object>
#' Database file:
#'   my_data.mod.db
#' Current table:
#'   mod_diff_windows
#' Connection:
#'   NULL
#' Last result: <matrix> [6 x 6]
#' }
#'
#' \code{last_result} is set by functions that compute an in-memory result
#' (\code{calc_mod_samplecor}, \code{plot_mod_pca}, \code{calc_mod_diff}).
#' Retrieve it with \code{get_mod_result(mod_db)} or \code{mod_db$last_result}.
#'
#' @return Called for its side effects (printing). Invisibly returns \code{mod_db}.
#'
#' @examples
#' \dontrun{
#' mod_db <- calc_mod_samplecor(mod_db)
#' print(mod_db)   # shows Last result: <matrix> [N x N]
#' }
#'
#' @importFrom DBI dbIsValid
#'
#' @export

print.mod_db <- function(mod_db) {
  cat("<mod_db object>\n")
  
  cat("Database file:\n")
  cat("  ", mod_db$db_file, "\n")
  
  cat("Current table:\n")
  cat("  ", ifelse(is.null(mod_db$current_table), "NULL", mod_db$current_table), "\n")
  
  cat("Connection:\n")

  if (is.character(mod_db$con) && mod_db$con == "none") {
    cat("  NULL\n")
  } else if (!dbIsValid(mod_db$con))
  {
    cat("  NULL\n")
  } else
  {
    cat("  Active DBI connection\n")
  }

  if (!is.null(mod_db$last_result)) {
    r <- mod_db$last_result
    dims <- if (is.data.frame(r) || is.matrix(r)) {
      paste0(" [", nrow(r), " x ", ncol(r), "]")
    } else if (is.list(r)) {
      paste0(" (", paste(names(r), collapse = ", "), ")")
    } else ""
    cat("Last result: <", class(r)[1], ">", dims, "\n", sep = "")
  }
}