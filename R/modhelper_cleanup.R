.modhelper_cleanup <- function(mod_db)
{
  # purge extra tables, update table list, and then close the connection
  mod_db$con <- .modhelper_purgeTables(mod_db$con)  # Purge tables FIRST
  dbExecute(mod_db$con, "VACUUM;")  # <-- Ensure space is reclaimed\
  mod_db <- .modhelper_closeDB(mod_db)     # Close DB LAST 
  
  invisible(mod_db)
}