#' Execute an Expression on a DuckDB Table with Optional Materialization
#'
#' Connects to a DuckDB database, evaluates a user-supplied expression on a specified table,
#' and either collects the result into R or computes and stores it as a new table in the database.
#'
#' @param mod_db Path to the DuckDB database file (e.g., `"my_data.mod.db"`).
#' @param table_name Name of the table in the database to operate on.
#' @param expr A function taking one argument (`tbl_ref`) and returning a lazy `dplyr` expression.
#'   The table reference (`tbl_ref`) will be passed as a `tbl()` object connected to the database.
#' @param mode One of `"collect"` (default) or `"compute"`. If `"collect"`, the result is returned as
#'   a data frame in R. If `"compute"`, the result is stored as a new table in the database.
#' @param output_table Required if `mode = "compute"`. Name of the output table to create or overwrite
#'   with the result of `expr(tbl_ref)`.
#'
#' @return If `mode = "collect"`, returns a data frame. If `mode = "compute"`, returns `NULL` invisibly.
#'
#' @examples
#' \dontrun{
#' # Collect results of a filtered table into R
#' run_mod_dplyr(
#'   mod_db = "my_data.mod.db",
#'   table_name = "methylation_data",
#'   expr = function(tbl_ref) dplyr::filter(tbl_ref, score > 0.5),
#'   mode = "collect"
#' )
#'
#' # Store the filtered result in a new table inside the database
#' run_mod_dplyr(
#'   mod_db = "my_data.mod.db",
#'   table_name = "methylation_data",
#'   expr = function(tbl_ref) dplyr::filter(tbl_ref, score > 0.5),
#'   mode = "compute",
#'   output_table = "filtered_data"
#' )
#' }
#' 
#' @importFrom DBI dbConnect dbDisconnect dbExecute dbListTables
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl collect compute
#' 
#' @export

run_mod_dplyr <- function(
    mod_db, 
    table_name, 
    expr, 
    mode = c("collect", "compute"), 
    output_table = NULL) 
{

  mode <- match.arg(mode)
  
  start_time <- Sys.time()
  # Connect to the database
  mod_db <- .modhelper_connectDB(mod_db)
  
  tbl_ref <- tbl(mod_db$con, table_name)
  
  result <- force(expr(tbl_ref))
  
  if (mode == "collect") {
    # Just collect the results into R
    out <- collect(result)
    
    end_time <- Sys.time()
    message("Query Finished. Time elapsed: ", end_time - start_time, "\n")
    mod_db <- .modhelper_closeDB(mod_db)
    
    return(out)
  } else if (mode == "compute") {
    if (is.null(output_table)) {
      stop("output_table must be provided when mode = 'compute'.", call. = FALSE)
    }
    
    # Drop existing table
    dbExecute(mod_db$con, paste0("DROP TABLE IF EXISTS ", output_table))
    
    # Compute into a new table
    computed <- compute(result, name = output_table, temporary = FALSE)
    
    end_time <- Sys.time()
    message("Query Finished. Time elapsed: ", end_time - start_time, "\n")
    mod_db <- .modhelper_closeDB(mod_db)
    
    invisible(mod_db)
  }
}

#' Execute a query on a mod Database
#'
#' Connects to a DuckDB database, evaluates a user-supplied sql query and closes the database.
#'
#' @param mod_db Path to the DuckDB database file (e.g., `"my_data.mod.db"`).
#' @param query An sql query supported by the duckdb database framework.
#'
#' @return a mod_db object to allow piping
#'
#' @examples
#' \dontrun{
#' # Count the number of rows in the calls table
#' run_mod_sql(
#'   mod_db = "my_data.mod.db",
#'   query = "CREATE TABLE call_count AS SELECT COUNT(*) AS num_rows FROM calls;")
#' }
#' 
#' @importFrom DBI dbExecute 
#' 
#' @export

run_mod_sql <- function(mod_db, 
                        query) 
{
  start_time <- Sys.time()
  # Connect to the database
  mod_db <- .modhelper_connectDB(mod_db)
  
  dbExecute(mod_db$con, query)
  
  end_time <- Sys.time()
  message("Query Finished. Time elapsed: ", end_time - start_time, "\n")
  mod_db <- .modhelper_closeDB(mod_db)
  
  invisible(mod_db)
}
