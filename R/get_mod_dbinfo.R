#' Get Database Statistics
#'
#' Prints a summary of the mod database, including file size, table names, and
#' unique sample names. Also stores a stats list in \code{mod_db$last_result}
#' so the information is accessible programmatically after a pipe step.
#'
#' @param mod_db A \code{"mod_db"} object or a character path to a \code{.mod.db} file.
#'
#' @return Invisibly returns a named list with elements \code{tables} (character
#'   vector of table names), \code{sample_names} (character vector), and
#'   \code{num_samples} (integer).
#'
#' @examples
#' \dontrun{
#' get_mod_dbinfo("my_data.mod.db")
#'
#' # Capture stats programmatically
#' info <- get_mod_dbinfo("my_data.mod.db")   # list(tables, sample_names, num_samples)
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbGetQuery dbListTables
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl distinct arrange collect pull
#'
#' @export

get_mod_dbinfo <- function(mod_db) 
{
  
  if (is.character(mod_db)) {
    # Check if the file exists
    if (!file.exists(mod_db)) {
      stop(paste("The database file", mod_db, "does not exist."))
    }
    
    cat("=================================================\n",
        "               Database Statistics               \n",
        "=================================================\n", 
        "Database: ", mod_db, "\n",
        sep = "")
  } else {
    cat("=================================================\n",
        "               Database Statistics               \n",
        "=================================================\n", 
        sep = "")
  }
  
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)
  
  # Get DB size
  size_df <- dbGetQuery(mod_db$con, "PRAGMA database_size")
  size <- sum(size_df$total_blocks * size_df$block_size) / 1024 / 1024
  cat(sprintf("\nDatabase Size: %.2f MB\n", size))
  
  # What tables are in the database?
  tables <- dbListTables(mod_db$con)
  cat("\nTables in Database:\n")
  cat(paste(tables, collapse = "\n"), "\n")
  
  # Unique Sample Names (if "calls" table exists)
  if ("calls" %in% tables) {
    sample_names <- tbl(mod_db$con, "calls") |>
      distinct(sample_name) |>
      arrange(sample_name) |>
      collect() |>
      pull(sample_name)
    
    cat("\nUnique Sample Names:\n")
    cat(paste(sample_names, collapse = "\n"), "\n")
    cat("\n")
  } else {
    cat("\nNo 'calls' table found in the database.\n")
    sample_names <- character(0)
  }
  
  # Assemble output list
  stats <- list(
    tables = tables,
    sample_names = sample_names,
    num_samples = length(sample_names)
  )
  
  .modhelper_closeDB(mod_db)
  invisible(stats)
}