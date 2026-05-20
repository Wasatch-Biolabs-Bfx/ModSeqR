#' Filter a DuckDB Table and Save to a New Table
#'
#' Applies filter conditions to a table in a DuckDB database and saves the result
#' to a new or replaced output table. This is done lazily using `dbplyr`, so the filtering
#' is translated to SQL and executed inside the database (not in R).
#'
#' @param mod_db Path to the DuckDB database file (e.g., `"my_data.mod.db"`).
#' @param input_table Name of the table to filter.
#' @param output_table Name of the output table to create or overwrite with the filtered results.
#' @param ... Filtering expressions (e.g., `score > 0.5`, `gene_id == "abc"`). These are unquoted expressions
#'   passed directly to `dplyr::filter()`.
#'
#' @return Invisibly returns the updated \code{"mod_db"} object. \code{last_result} is set to a
#'   tibble with columns \code{sample_name} and \code{n} (row count per sample) if the output
#'   table contains a \code{sample_name} column, otherwise a single integer row count.
#'
#' @examples
#' \dontrun{
#' filter_mod_table(mod_db = train_db, 
#' input_table = "collapsed_windows", 
#' output_table = "collapsed_windows",
#' !(chrom %in% c("chrX", "chrY")))
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExecute
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl filter
#' @importFrom dbplyr sql_render
#' @importFrom rlang enquos
#' 
#' @export

filter_mod_table <- function(mod_db, input_table, output_table, ...) {
  start_time <- Sys.time()
  
  if (missing(output_table)) {
    stop("Please specify an output_table name.")
  }
  
  # Capture filter conditions
  conditions <- enquos(...)
  
  # Connect to DuckDB
  mod_db <- .modhelper_connectDB(mod_db)
  
  # Create a lazy DuckDB table
  tbl_expr <- tbl(.get_con(mod_db), input_table)
  # Apply filters lazily via dbplyr (this generates SQL, not local evaluation)
  filtered_expr <- tryCatch({
    filter(tbl_expr, !!!conditions)
  }, error = function(e) {
    stop("Error in filter conditions: ", e$message)
  })
  
  # Render SQL
  sql_query <- sql_render(filtered_expr) |> as.character()
  
  # Stage results in a persistent (not TEMP) table so DuckDB can page it to disk.
  temp_table <- paste0("_filter_staging_", as.integer(Sys.time()))
  dbExecute(.get_con(mod_db), sprintf("DROP TABLE IF EXISTS %s", temp_table))
  dbExecute(.get_con(mod_db), sprintf("CREATE TABLE %s AS %s", temp_table, sql_query))

  # Write to output_table, then clean up staging table
  dbExecute(.get_con(mod_db), sprintf("CREATE OR REPLACE TABLE %s AS SELECT * FROM %s", output_table, temp_table))
  dbExecute(.get_con(mod_db), sprintf("DROP TABLE IF EXISTS %s", temp_table))
  
  cat("\n")
  end_time <- Sys.time()
  
  message("Filter complete! Time elapsed: ", end_time - start_time, "\n")

  cols <- DBI::dbListFields(.get_con(mod_db), output_table)
  mod_db$last_result <- if ("sample_name" %in% cols) {
    dplyr::tbl(.get_con(mod_db), output_table) |> dplyr::count(sample_name) |> dplyr::collect()
  } else {
    DBI::dbGetQuery(.get_con(mod_db), sprintf("SELECT COUNT(*) AS n FROM %s", output_table))$n
  }

  mod_db$current_table <- output_table
  mod_db <- .modhelper_cleanup(mod_db)
  invisible(mod_db)
}