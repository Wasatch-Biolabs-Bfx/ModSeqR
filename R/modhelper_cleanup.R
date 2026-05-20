.modhelper_cleanup <- function(mod_db)
{
  .modhelper_purgeTables(.get_con(mod_db))
  DBI::dbExecute(.get_con(mod_db), "VACUUM;")
  invisible(mod_db)
}
