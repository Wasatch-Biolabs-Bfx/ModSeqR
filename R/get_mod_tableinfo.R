#' Get Table Information from mod Database
#'
#' Provides a summary of a specified table within a `.mod.db` database, including total number of records,
#' sample-level record counts (if applicable), and column names.
#'
#' @param mod_db Path to the `.mod.db` file or a `mod_db` object.
#' @param table_name Name of the table to summarize.
#'
#' @examples
#' # Specify the path to the database
#' mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
#'
#' # Get information about the 'calls' table
#' get_mod_tableinfo(mod_db = mod_db, table_name = "calls")
#'
#' @return Invisibly returns the closed `mod_db` object after summarizing the specified table.
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