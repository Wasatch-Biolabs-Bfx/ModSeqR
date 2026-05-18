#' Preview the First Rows of a Database Table
#'
#' Prints and captures the first \code{n} rows of a table in the \code{.mod.db}
#' database. Defaults to the \code{current_table} when \code{table_name} is
#' omitted, so it fits naturally at any point in a pipe.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#' @param table_name Character. Name of the table to preview. Defaults to
#'   \code{mod_db$current_table}.
#' @param n Integer. Number of rows to return. Default: \code{6}.
#'
#' @return Invisibly returns \code{mod_db} with \code{last_result} set to the
#'   collected preview tibble. Retrieve it with \code{get_mod_result(mod_db)} or
#'   \code{mod_db$last_result}.
#'
#' @examples
#' \dontrun{
#' # Peek at the current table mid-pipe
#' mod_db <- make_mod_db(ch3_files, "my_db") |>
#'   summarize_mod_windows() |>
#'   peek_mod_table()
#'
#' # Peek at a specific table
#' peek_mod_table(mod_db, "mod_windows", n = 10)
#'
#' # Capture the preview
#' mod_db  <- peek_mod_table(mod_db)
#' preview <- get_mod_result(mod_db)
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
      mod_db <- .modhelper_closeDB(mod_db)
      stop("No table_name supplied and mod_db$current_table is NULL.")
    }
  }

  if (!(table_name %in% DBI::dbListTables(mod_db$con))) {
    mod_db <- .modhelper_closeDB(mod_db)
    stop(paste0("Table '", table_name, "' does not exist in the database."))
  }

  result <- dplyr::tbl(mod_db$con, table_name) |>
    head(n) |>
    dplyr::collect()

  cat("Table:", table_name, "(first", n, "rows)\n")
  print(result)

  mod_db$last_result <- result
  mod_db <- .modhelper_closeDB(mod_db)
  invisible(mod_db)
}
