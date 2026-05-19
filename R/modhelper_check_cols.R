#' Check that required columns are present in a database table
#'
#' Stops with an informative error if any required columns are missing.
#'
#' @param con A DBI connection.
#' @param table_name Character. Name of the table to check.
#' @param required_cols Character vector of required column names.
#'
#' @return Invisibly returns \code{TRUE} if all columns are present.
#'
#' @keywords internal

.modhelper_check_cols <- function(con, table_name, required_cols) {
  present <- DBI::dbListFields(con, table_name)
  missing <- setdiff(required_cols, present)
  if (length(missing) > 0) {
    stop(sprintf(
      "Table '%s' is missing required column(s): %s\nAvailable columns: %s",
      table_name,
      paste(missing, collapse = ", "),
      paste(present, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

#' Detect the logical type of a mod database table from its columns
#'
#' Returns one of \code{"regions"}, \code{"windows"}, \code{"positions"},
#' \code{"mod_diff"}, \code{"reads"}, or \code{"calls"}.
#'
#' @param con A DBI connection.
#' @param table_name Character. Name of the table to inspect.
#'
#' @return Character scalar.
#'
#' @keywords internal

.modhelper_detect_table_type <- function(con, table_name) {
  cols <- DBI::dbListFields(con, table_name)
  if ("meth_diff"   %in% cols) return("mod_diff")
  if ("region_name" %in% cols) return("regions")
  if ("num_sites"   %in% cols) return("windows")
  if ("call_prob"   %in% cols) return("calls")
  if ("read_id"     %in% cols) return("reads")
  return("positions")
}
