#' Connect to a Database
#'
#' This internal function establishes a connection to a DuckDB database. It can handle both a character file name 
#' or an object of class `mod_db` to open the database.
#'
#' @param mod_db A character string representing the file path to the DuckDB database or an object of class `mod_db`.
#'
#' @details
#' This function checks the class of `mod_db` and attempts to connect to the database. If `mod_db` is a character string, 
#' it will create an object of class `mod_db`. If `mod_db` is already of class `mod_db`, it will directly establish a 
#' connection to the database.
#'
#' @note This function is intended for internal use within the package.
#' 
#' @return A database connection object.
#' 
#' @importFrom DBI dbConnect dbListTables
#' @importFrom duckdb duckdb
#' @importFrom withr defer
#'
#' @keywords internal
#' 

.modhelper_connectDB <- function(mod_db)
{
  # Compute resource caps and prepare spill directory before opening the connection.
  # Passing config= to duckdb() sets memory_limit and temp_directory at the engine
  # level — before any query runs — so spill-to-disk is always active as a backstop.
  caps     <- .auto_duckdb_resource_caps(0.75)
  tmp_path <- file.path(tempdir(), "modseqr_duckdb_tmp")
  dir.create(tmp_path, recursive = TRUE, showWarnings = FALSE)
  cfg <- list(
    memory_limit   = caps$memory_limit,
    temp_directory = tmp_path,
    threads        = as.character(caps$threads)
  )

  if (inherits(mod_db, "character")) {
    if (!grepl(".mod.db$", mod_db)) mod_db <- paste0(mod_db, ".mod.db")
    database <- list(db_file = mod_db, current_table = NULL, con = NULL, last_result = NULL)
    class(database) <- "mod_db"
    database$con <- dbConnect(duckdb(database$db_file, config = cfg), read_only = FALSE)
    defer(.modhelper_closeDB(database), parent.frame())
    return(database)

  } else if (inherits(mod_db, "mod_db")) {
    mod_db$con <- dbConnect(duckdb(mod_db$db_file, config = cfg), read_only = FALSE)
    defer(.modhelper_closeDB(mod_db), parent.frame())
    return(mod_db)

  } else {
    stop("Invalid mod_db class. Must be character or mod_db.")
  }
}