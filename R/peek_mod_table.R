#' Preview the First Rows of a Database Table
#'
#' Prints and returns the first \code{n} rows of a table in the \code{.mod.db}
#' database. Defaults to \code{mod_db$current_table} when \code{table_name} is
#' omitted.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#' @param table_name Character. Name of the table to preview. Defaults to
#'   \code{mod_db$current_table}.
#' @param n Integer. Number of rows to return. Default: \code{6}.
#'
#' @return Invisibly returns a tibble of the first \code{n} rows.
#'
#' @examples
#' \dontrun{
#' # Print and capture a preview
#' preview <- peek_mod_table(mod_db, "mod_windows", n = 10)
#'
#' # Use current_table default
#' preview <- peek_mod_table(mod_db)
#' }
#'
#' @importFrom DBI dbListTables
#' @importFrom dplyr tbl collect
#'
#' @export

peek_mod_table <- function(mod_db, table_name = NULL, n = 6) {

  mod_db <- .modhelper_connectDB(mod_db)

  if (is.null(table_name)) {
    table_name <- mod_db$current_table
    if (is.null(table_name)) {
      .modhelper_closeDB(mod_db)
      stop("No table_name supplied and mod_db$current_table is NULL.")
    }
  }

  if (!(table_name %in% DBI::dbListTables(mod_db$con))) {
    .modhelper_closeDB(mod_db)
    stop(paste0("Table '", table_name, "' does not exist in the database."))
  }

  result <- dplyr::tbl(mod_db$con, table_name) |>
    head(n) |>
    dplyr::collect()

  cat("Table:", table_name, "(first", n, "rows)\n")
  print(result)

  .modhelper_closeDB(mod_db)
  invisible(result)
}
