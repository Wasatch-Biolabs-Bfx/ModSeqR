#' Collapse Windows Based on Methylation Differences
#'
#' This function collapses significant windows in a methylation dataset by merging
#' contiguous regions that meet the specified criteria. Can only collapse windows 
#' once a differential modification analysis (calc_mod_diff()) has been called.
#'
#' @param mod_db A DuckDB database connection object or path to the database.
#' @param input_table Character. Name of the database table containing
#'        differential methylation windows to be collapsed. The table must
#'        contain at least the columns \code{chrom}, \code{start}, \code{end},
#'        \code{p_adjust}, and \code{meth_diff}. Default is \code{"mod_diff_windows"}.
#' @param table_name Character. Name of the output table to store collapsed
#'        windows (default: "collapsed_windows").
#' @param max_distance Numeric. The maximum allowable distance between consecutive
#'        significant windows for merging (default: 1000).
#' @param sig_cutoff Numeric. The significance threshold for adjusted p-values
#'        (default: 0.05).
#' @param min_diff Numeric. The minimum absolute methylation difference required
#'        for inclusion in the analysis (default: 0.5).
#'
#' @return Invisibly returns the updated \code{"mod_db"} object with
#'   \code{current_table} set to \code{table_name}. The collapsed results are
#'   written to \code{table_name} inside the database.
#'
#' @details
#' The function performs the following steps:
#' \itemize{
#'   \item Filters the `mod_diff_windows` to retain only significant windows where
#'         `p_adjust <= sig_cutoff` and `ABS(meth_diff) >= min_diff`.
#'   \item Assigns a new region identifier based on proximity (`max_distance`) and
#'         the direction of methylation differences.
#'   \item Collapses regions by grouping contiguous windows, computing the
#'         average methylation difference (`avg_meth_diff`), and counting the
#'         number of merged windows.
#' }
#'
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable dbExecute
#' @importFrom duckdb duckdb
#' @importFrom glue glue
#' 
#' @export

collapse_mod_windows <- function(mod_db, 
                                 input_table = "mod_diff_windows",
                                 table_name  = "collapsed_windows",
                                 max_distance = 1000,
                                 sig_cutoff = 0.05,
                                 min_diff = 0.5) 
{
  start_time <- Sys.time()
  mod_db <- .modhelper_connectDB(mod_db)
  
  # Quote identifiers safely for DuckDB
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(mod_db$con, x))
  
  # ---- Check input table exists ----
  if (!DBI::dbExistsTable(mod_db$con, input_table)) {
    stop(glue::glue(
      "Error: Table '{input_table}' not found in the database.\n",
      "Please run 'calc_mod_diff()' (or equivalent) to generate a differential windows table first.\n"
    ))
  }
  
  # ---- Validate required columns exist in the chosen input table ----
  cols <- DBI::dbListFields(mod_db$con, input_table)
  required <- c("chrom", "start", "end", "p_adjust", "meth_diff")
  missing <- setdiff(required, cols)
  if (length(missing) > 0) {
    stop(glue::glue(
      "Error: Input table '{input_table}' is missing required columns: ",
      paste(missing, collapse = ", "),
      ".\nThis function expects differential-window output with at least: ",
      paste(required, collapse = ", "),
      ".\n"
    ))
  }
  
  cat("Collapsing windows on differential analysis results from: ", input_table, "\n", sep = "")
  
  # --- Build AVG(...) list dynamically based on existing columns ---
  patterns <- c("_counts_control$", "_counts_case$", "_frac_control$", "_frac_case$")
  match_cols <- cols[grepl(paste(patterns, collapse = "|"), cols)]
  
  avg_exprs <- if (length(match_cols)) {
    paste0("AVG(", qi(match_cols), ") AS ", qi(paste0("avg_", match_cols)))
  } else character(0)
  
  avg_select_sql <- if (length(avg_exprs)) {
    paste0(",\n      ", paste(avg_exprs, collapse = ",\n      "))
  } else {
    ""
  }
  
  query <- glue::glue(
    "CREATE OR REPLACE TABLE {qi(table_name)} AS
     WITH FilteredWindows AS (
       SELECT *
       FROM {qi(input_table)}
       WHERE {qi('p_adjust')} <= {sig_cutoff}
         AND ABS({qi('meth_diff')}) >= {min_diff}
     ),
     NumberedWindows AS (
       SELECT *,
         CASE
           WHEN LAG({qi('end')}) OVER w IS NULL
                OR LAG({qi('end')}) OVER w + {max_distance} < {qi('start')}
                OR SIGN({qi('meth_diff')}) != SIGN(LAG({qi('meth_diff')}) OVER w) THEN 1
           ELSE 0
         END AS new_region_flag
       FROM FilteredWindows
       WINDOW w AS (PARTITION BY {qi('chrom')} ORDER BY {qi('start')})
     ),
     RegionGroups AS (
       SELECT *,
         SUM(new_region_flag) OVER (
           PARTITION BY {qi('chrom')} ORDER BY {qi('start')}
           ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW
         ) AS region_id
       FROM NumberedWindows
     )
     SELECT
       {qi('chrom')},
       MIN({qi('start')}) AS {qi('start')},
       MAX({qi('end')})   AS {qi('end')}
       {avg_select_sql}
       , AVG({qi('meth_diff')}) AS avg_meth_diff
       , COUNT(*) AS num_windows
     FROM RegionGroups
     GROUP BY {qi('chrom')}, region_id
     ORDER BY {qi('chrom')}, {qi('start')};"
  )
  
  DBI::dbExecute(mod_db$con, query)
  
  end_time <- Sys.time()
  total_seconds <- as.numeric(end_time - start_time, units = "secs")
  
  if (total_seconds > 60) {
    message("Windows successfully collapsed - ", table_name, " created!",
            "\nTime elapsed: ", round(total_seconds/60, 2), " minutes\n")
  } else {
    message("Windows successfully collapsed - ", table_name, " created!",
            "\nTime elapsed: ", round(total_seconds, 2), " seconds\n")
  }
  
  mod_db$current_table <- table_name
  mod_db <- .modhelper_cleanup(mod_db)
  invisible(mod_db)
}

