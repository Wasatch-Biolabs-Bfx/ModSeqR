#' Collect Table from DuckDB Database as Tibble
#'
#' This function connects to a DuckDB database and collects a specified table as a tibble.
#'
#' @param mod_db A list containing the database file path. This should be a valid "mod_db" class object.
#' @param table_name A string representing the name of the table to collect from the database.
#' @param max_rows The maximum amount of rows wanted for calculation. This argument can help analysis run faster when there is a lot of data.
#'
#' @details
#' The function establishes a connection to the DuckDB database using \code{.modhelper_connectDB}.
#' It retrieves the specified table as a tibble. If the table does not exist,
#' a message is printed and an empty tibble is returned.
#'
#' @return A tibble containing the collected data from the specified database table. If the table retrieval fails, an empty tibble is returned.
#'
#' @examples
#' \dontrun{
#' mod_db <- "my_data.mod.db"
#' positions <- get_mod_table(mod_db, "positions")
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbListTables
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl collect slice_sample
#' @importFrom tibble tibble
#' 
#' @export

get_mod_table <- function(mod_db, 
                      table_name,
                      max_rows = NULL)
{
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)
  
  dat <- tibble()  # Initialize an empty tibble to return if there's an error
  
  if (table_name %in% dbListTables(.get_con(mod_db))) {
    # Reference the table
    table_ref <- tbl(.get_con(mod_db), table_name)
    
    if (!is.null(max_rows)) {
      # Randomly sample max_rows rows (efficient with DuckDB)
      dat <- table_ref |> slice_sample(n = max_rows) |> collect()
    } else {
      dat <- table_ref |> collect()
    }
  } else {
    message(paste0("Table '", table_name, "' does not exist in the database."))
  }
  
  return(dat)
}