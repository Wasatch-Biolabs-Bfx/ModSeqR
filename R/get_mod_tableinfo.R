#' Get Table Information from mod Database
#'
#' Prints a summary of a specified table in a \code{.mod.db} database, including
#' total record count, per-sample record counts (when a \code{sample_name} column
#' exists), and column names.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#' @param table_name Character. Name of the table to summarize.
#'
#' @return Invisibly returns \code{mod_db} after printing the table summary.
#'
#' @examples
#' \dontrun{
#' get_mod_tableinfo("my_data.mod.db", table_name = "mod_windows")
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables dbListFields
#'
#' @export

get_mod_tableinfo <- function(mod_db, 
                              table_name) 
{
  if (is.character(mod_db)) if (!file.exists(mod_db)) {
    stop(paste("The database file", mod_db, " does not exist."))
  }
  
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)
  tables <- DBI::dbListTables(mod_db$con)
  
  # If the table does not exist
  if (!(table_name %in% tables)) {
    cat("Table '", table_name, "' does not exist in the database.\n", sep = "")
    mod_db <- .modhelper_closeDB(mod_db)
    return(invisible(mod_db))
  }
  
  # Else, print db stats
  cat("=================================================\n",
      "                 Table Statistics                \n",
      "=================================================\n",
      sep = "")
  
  cat("Table:", table_name, "\n")
  
  # Total number of records
  num_records <- DBI::dbGetQuery(mod_db$con, paste0("SELECT COUNT(*) AS n FROM ", table_name))$n
  
  cat("\nTotal Number of Records:", num_records, "\n")
  
  # Records by sample_name (if applicable)
  col_names <- DBI::dbListFields(mod_db$con, table_name)
  
  if ("sample_name" %in% col_names) {
    sample_counts <- DBI::dbGetQuery(
      mod_db$con,
      paste0("SELECT sample_name, COUNT(*) AS n FROM ", table_name,
             " GROUP BY sample_name ORDER BY sample_name")
    )
    
    apply(sample_counts, 1, function(row) {
      cat(sprintf("  %s: %d\n", row[["sample_name"]], as.integer(row[["n"]])))
    })
  }
  
  # Indented column names
  cat("\nColumns:\n")
  for (col in col_names) {
    cat("  ", col, "\n")
  }
  
  mod_db <- .modhelper_closeDB(mod_db)
  invisible(mod_db)
}