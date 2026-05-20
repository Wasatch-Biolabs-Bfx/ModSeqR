#' Get Table Information from mod Database
#'
#' Prints a summary of a specified table in a \code{.mod.db} database, including
#' total record count, per-sample record counts (when a \code{sample_name} column
#' exists), and column names.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#' @param table_name Character. Name of the table to summarize.
#'
#' @return Invisibly returns a named list with elements \code{table} (table name),
#'   \code{num_records} (integer), \code{sample_counts} (data frame or \code{NULL}),
#'   and \code{columns} (character vector of column names).
#'
#' @examples
#' \dontrun{
#' get_mod_tableinfo("my_data.mod.db", table_name = "mod_windows")
#'
#' # Capture programmatically
#' info <- get_mod_tableinfo(mod_db, "mod_windows")
#' info$num_records
#' info$columns
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables dbListFields
#'
#' @export

get_mod_tableinfo <- function(mod_db,
                              table_name)
{
  if (is.character(mod_db)) if (!file.exists(mod_db)) {
    stop(paste("The database file", mod_db, "does not exist."))
  }

  mod_db <- .modhelper_connectDB(mod_db)
  tables <- DBI::dbListTables(.get_con(mod_db))

  if (!(table_name %in% tables)) {
    cat("Table '", table_name, "' does not exist in the database.\n", sep = "")
    return(invisible(NULL))
  }

  cat("=================================================\n",
      "                 Table Statistics                \n",
      "=================================================\n",
      sep = "")

  cat("Table:", table_name, "\n")

  num_records <- DBI::dbGetQuery(
    .get_con(mod_db), paste0("SELECT COUNT(*) AS n FROM ", table_name))$n

  cat("\nTotal Number of Records:", num_records, "\n")

  col_names     <- DBI::dbListFields(.get_con(mod_db), table_name)
  sample_counts <- NULL

  if ("sample_name" %in% col_names) {
    sample_counts <- DBI::dbGetQuery(
      .get_con(mod_db),
      paste0("SELECT sample_name, COUNT(*) AS n FROM ", table_name,
             " GROUP BY sample_name ORDER BY sample_name")
    )
    apply(sample_counts, 1, function(row) {
      cat(sprintf("  %s: %d\n", row[["sample_name"]], as.integer(row[["n"]])))
    })
  }

  cat("\nColumns:\n")
  for (col in col_names) cat("  ", col, "\n")

  info <- list(
    table         = table_name,
    num_records   = num_records,
    sample_counts = sample_counts,
    columns       = col_names
  )

  invisible(info)
}
