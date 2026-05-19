#' Export a Table from the mod Database to CSV
#'
#' Exports a single table from a mod database to a CSV file using DuckDB's
#' native \code{COPY} command (efficient; does not pull data into R).
#'
#' @param mod_db A string. The path to the \code{.mod.db} DuckDB database.
#' @param table A string specifying the table to export from the database.
#'   Default is \code{"positions"}.
#' @param out_path A string. Either a directory path (the file is named
#'   \code{<table>.csv} automatically) or a full \code{.csv} file path.
#'
#' @details
#' The function connects to the database, checks that \code{table} exists,
#' and runs a \code{COPY ... TO} statement entirely inside DuckDB.
#' If \code{out_path} does not end in \code{.csv}, the table name is appended
#' as the filename.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return).
#' 
#' @importFrom DBI dbExistsTable dbQuoteIdentifier dbQuoteString dbExecute
#'
#' @export

export_mod_table <- function(mod_db, 
                             table = "positions", 
                             out_path) {
  # Open DB connection
  mod_db <- .modhelper_connectDB(mod_db)
  on.exit({ .modhelper_closeDB(mod_db) }, add = TRUE)
  
  # Build output path
  if (!grepl("\\.csv$", out_path, ignore.case = TRUE)) {
    if (!grepl("/$", out_path)) out_path <- paste0(out_path, "/")
    out_path <- paste0(out_path, table, ".csv")
  }
  
  if (!DBI::dbExistsTable(mod_db$con, table)) {
    message("Table '", table, "' does not exist.")
    return(invisible(mod_db))
  }
  
  tbl_id  <- as.character(DBI::dbQuoteIdentifier(mod_db$con, table))
  file_q  <- as.character(DBI::dbQuoteString(mod_db$con, out_path))
  
  # COPY is executed entirely inside DuckDB (fast + avoids R memory)
  sql <- sprintf(
    "COPY (SELECT * FROM %s) TO %s WITH (FORMAT CSV, HEADER, DELIMITER ',', QUOTE '\"');",
    tbl_id, file_q
  )
  
  message("Exporting '", table, "' to: ", out_path)
  n <- DBI::dbExecute(mod_db$con, sql)
  message("Rows written: ", n)

  mod_db$last_result <- out_path
  invisible(mod_db)
}
