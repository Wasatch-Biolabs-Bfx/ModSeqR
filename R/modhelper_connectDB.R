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
  # Open the database connection
  if (inherits(mod_db, "character")) { # if given a string file name- create a new database
    # Open the database connection - first check to make sure correct name is there
    if (is.character(mod_db)) {
      if (!grepl(".mod.db$", mod_db)) {
        mod_db <- paste0(mod_db, ".mod.db")
      }
    }
    # make mod_object
    database <- list(db_file = mod_db, current_table = NULL, con = NULL)
    class(database) <- "mod_db"
    
    #add in the connection
    database$con <- dbConnect(duckdb(database$db_file), read_only = FALSE)
    defer(.modhelper_closeDB(database), parent.frame())
    
    # return database object
    return(database)
  
  } else if (inherits(mod_db, "mod_db")) { # if given a mod_db OBJECT!
    mod_db$con <- dbConnect(duckdb(mod_db$db_file), read_only = FALSE)
    defer(.modhelper_closeDB(mod_db), parent.frame())
    
    return(mod_db)
  } else {
    stop("Invalid mod_db class. Must be character or mod_db.")
  }
  
}