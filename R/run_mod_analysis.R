#' Run Differential Analysis on Methylation Data
#'
#' This function performs a differential analysis on methylation data based on the specified input
#' table and applies the appropriate summarization method. It supports analysis of positions, regions, or windows.
#' The function handles the summarization of methylation data and performs differential modification analysis
#' based on case-control comparisons.
#'
#' @param mod_db A `mod_db` object representing the DuckDB database containing methylation data.
#'   The database should include necessary tables for the analysis, such as positions, regions, or windows.
#' @param out_path The directory in which the "Mod_Diff_Analysis_Results" directory containing result data will be written out too.
#' If the user does not provide a directory, the working directory will be used.
#' @param input_table A character string specifying the name of the table to analyze, or one of the
#'   default types: \code{"positions"}, \code{"regions"}, or \code{"windows"}. If the table already
#'   exists in the database, summarization is skipped and its type is detected automatically.
#'   If it does not exist and the value is a known default type, the appropriate summarization
#'   function is run first.
#' @param region_file A character string specifying the path to the region annotation file (required if `input_table` is `"regions"`).
#'   This file should be in a supported format (e.g., BED, CSV, TSV).
#' @param window_size An integer specifying the window size for summarizing methylation data if `input_table` is `"windows"`.
#'   The default value is 1000.
#' @param step_size An integer specifying the step size for sliding windows if `input_table` is `"windows"`.
#'   The default value is 10.
#' @param cases A character vector of sample names to be used as cases in the differential analysis.
#'   This argument is required and cannot be NULL.
#' @param controls A character vector of sample names to be used as controls in the differential analysis.
#'   This argument is required and cannot be NULL.
#' @param mod_type A character string specifying the modification type to analyze. The default is `"mh"`,
#'   which includes both methylation and hydroxymethylation.
#'   Other options are `"c"` for unmodified cytosine, `"m"` for methylation, and `"h"` for hydroxymethylation.
#' @param calc_type A character string specifying the type of statistical test to use for the differential analysis.
#'   The default is `"fast_fisher"`, but other calculation methods can be implemented.
#' @param p_val_max The p value threshold in which significant differentially modified positions/regions/windows
#' will be written out in the final directory.
#' @param call_type Deprecated. Use \code{input_table} instead.
#'
#' @details
#' This function first checks whether the specified \code{input_table} already exists in the database.
#' If it does, summarization is skipped and the table type is auto-detected from its columns.
#' If it does not exist and \code{input_table} is one of \code{"positions"}, \code{"regions"}, or
#' \code{"windows"}, the appropriate \code{summarize_mod_*()} function is called first.
#' It then proceeds with a differential modification analysis between the provided case and control samples.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return).
#'   \code{last_result} is set to a named character vector of exported file paths:
#'   \code{mod_diff} (full differential results), \code{all_cpgs} (all CpG data), and
#'   \code{sig_cpgs} (significant sites only).
#'
#' @importFrom dplyr select filter matches semi_join
#' @importFrom tidyr pivot_longer pivot_wider unite
#' @importFrom utils write.csv
#' @importFrom stats setNames
#' @importFrom fs dir_create
#'
#' @export

run_mod_analysis <- function(mod_db,
                         out_path,
                         input_table,
                         region_file = NULL,
                         window_size = 1000,
                         step_size = 10,
                         cases,
                         controls,
                         mod_type = "mh",
                         calc_type = "fast_fisher",
                         p_val_max = 0.05,
                         call_type = NULL) {

  if (!is.null(call_type)) {
    warning("'call_type' is deprecated; use 'input_table' instead.", call. = FALSE)
    input_table <- call_type
  }

  if (missing(input_table) || is.null(input_table)) {
    stop("Error: The 'input_table' argument is required and cannot be NULL. Please specify 'positions', 'regions', 'windows', or an existing table name.")
  }

  if (missing(cases) || is.null(cases)) {
    stop("Error: The 'cases' argument is required and cannot be NULL. Please specify which samples are to be cases for differential analysis.")
  }

  if (missing(controls) || is.null(controls)) {
    stop("Error: The 'controls' argument is required and cannot be NULL. Please specify which samples are to be controls for differential analysis.")
  }

  cat("\nRunning Analyses...\n")
  # Record the start time
  start_time <- Sys.time()

  # Check whether the table already exists in the database
  tmp_db <- .modhelper_connectDB(mod_db)
  table_exists <- DBI::dbExistsTable(tmp_db$con, input_table)

  if (table_exists) {
    # Table already exists — skip summarization, detect type
    table_type <- .modhelper_detect_table_type(tmp_db$con, input_table)
    cat(paste0("Table '", input_table, "' found in database (type: ", table_type, "). Skipping summarization.\n"))
  } else {
    # Table does not exist — run appropriate summarization
    if (input_table == "positions") {
      summarize_mod_positions(mod_db)
      table_type <- "positions"
    } else if (input_table == "regions") {
      if (is.null(region_file)) {
        stop(paste("Error: region annotation file missing.\n
                 Please include path to annotation file in the region_file argument of this function.\n"))
      }
      summarize_mod_regions(mod_db, region_file = region_file)
      table_type <- "regions"
    } else if (input_table == "windows") {
      summarize_mod_windows(mod_db, window_size = window_size, step_size = step_size)
      table_type <- "windows"
    } else {
      stop(paste0("Error: Table '", input_table, "' does not exist in the database and is not a known default type.\n",
                  "Please specify 'positions', 'regions', or 'windows', or ensure the table exists first."))
    }
  }

  diff_table_name <- paste0("mod_diff_", input_table)

  # Second, run differential modification analysis...
  cat("\nBeginning Differential Analysis...\n")
  calc_mod_diff(mod_db,
                input_table,
                cases,
                controls,
                mod_type,
                calc_type)

  # Third, create directory for user with meth_diff table, all methylation data, and the significant regions/windows/positions meth data...
  if (is.null(out_path)) {
    out_path = getwd()
  }

  cat(paste0("\nWriting out differential analysis results to ", out_path, "\n"))

  new_dir <- file.path(out_path, "Mod_Diff_Analysis_Results")

  if (!dir.exists(new_dir)) {
    dir.create(new_dir, recursive = TRUE)
  }

  # Write out the mod_diff table from analysis
  mod_diff_path <- file.path(new_dir, "mod_diff.csv")
  # Write the data frame to a CSV file DIRECTLY from the database
  # Connect to the database
  mod_db <- .modhelper_connectDB(mod_db)

  dbExecute(
    .get_con(mod_db),
    glue("COPY {diff_table_name} TO '{mod_diff_path}' (HEADER, DELIMITER ',')")
  )

  # Write out all methylation data from all samples
  cat("\nWriting out all CpG data...\n")

  all_CGs_path <- file.path(new_dir, "All_CpGs.csv")

  dbExecute(
    .get_con(mod_db),
    glue("COPY {input_table} TO '{all_CGs_path}' (HEADER, DELIMITER ',')")
  )

  # Write out only the significant regions of interests methylation data...
  cat("\nWriting out significant CpG data based on differential analysis...\n")
  cat(paste0("p-value threshold: ", p_val_max, "\n"))

  Sig_CGs_path <- file.path(new_dir, "Sig_CpGs.csv")

  # automatically collapse if windows is selected
  if (table_type == "windows") {
    collapse_mod_windows(mod_db)

    dbExecute(
      .get_con(mod_db),
      glue("COPY collapsed_windows TO '{Sig_CGs_path}' (HEADER, DELIMITER ',')")
    )
  } else{
    sql <- glue("
    COPY (
      SELECT call.*
      FROM {`input_table`} AS call
      JOIN {`diff_table_name`} AS diff
      ON call.chrom = diff.chrom
         AND call.start = diff.start
         AND call.\"end\" = diff.\"end\"
      WHERE diff.p_val <= {p_val_max}
    ) TO '{Sig_CGs_path}' (HEADER, DELIMITER ',')
  ")

    dbExecute(.get_con(mod_db), sql)
  }

  cat("\nRun Analysis Complete!\n")

  mod_db$last_result <- c(
    mod_diff = mod_diff_path,
    all_cpgs = all_CGs_path,
    sig_cpgs = Sig_CGs_path
  )

  # Record the end time
  end_time <- Sys.time()

  total_time_difftime <- end_time - start_time

  # Convert the total_time_difftime object to numeric seconds for a reliable comparison
  total_seconds <- as.numeric(total_time_difftime, units = "secs")

  if (total_seconds > 60) {
    # If greater than 60 seconds, convert to numeric minutes for display
    total_minutes <- as.numeric(total_time_difftime, units = "mins")
    message("Time elapsed: ", round(total_minutes, 2), " minutes\n")
  } else {
    # Otherwise, display in numeric seconds
    message("Time elapsed: ", round(total_seconds, 2), " seconds\n")
  }

  invisible(mod_db)
}
