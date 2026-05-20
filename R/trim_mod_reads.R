#' Trim methylation calls at the read ends in a mod database
#'
#' Creates a new table in a mod database that keeps only methylation calls
#' within a central fraction of each read (i.e., trims a fraction from the
#' start and/or end of each read).
#'
#' If \code{samples} is \code{NULL} (default), trimming is applied to ALL samples.
#' If \code{samples} is provided, trimming is applied ONLY to those sample(s),
#' and ALL rows from other samples are retained unchanged in the output table.
#'
#' @param mod_db An object identifying the mod database, as accepted by
#'   \code{ModSeqR:::.modhelper_connectDB()} (e.g. a file path or an
#'   existing mod database object).
#' @param by_frac Numeric scalar between 0 and 1 (exclusive) giving the
#'   fraction of the read length to trim from each enabled end.
#'   For example, \code{by_frac = 0.1} trims the first/last 10% depending
#'   on \code{trim_start}/\code{trim_end}.
#' @param trim_start Logical; whether to trim from the start (low
#'   \code{read_position}) of each read.
#' @param trim_end Logical; whether to trim from the end (high
#'   \code{read_position}) of each read.
#' @param input_table Character scalar; name of the input table
#'   containing methylation calls (default \code{"calls"}). Must contain
#'   at least \code{read_position} and \code{read_length} columns.
#' @param output_table Character scalar; name of the output table to
#'   create/overwrite with the trimmed calls (default \code{"calls_trimmed"}).
#' @param samples Character vector of sample name(s) to trim. If \code{NULL}
#'   (default), trims all samples. If non-NULL, requires a \code{sample_name}
#'   column in \code{input_table}. Only these samples are trimmed; all other
#'   samples are copied through unchanged.
#'
#' @return Invisibly returns the updated \code{mod_db} object, with
#'   \code{current_table} set to \code{output_table}. \code{last_result} is set to a tibble
#'   with columns \code{sample_name} and \code{n} (row count per sample).
#'
#' @examples
#' \dontrun{
#' # Trim last 10% of reads for all samples
#' trim_mod_reads(mod_db, by_frac = 0.1, trim_end = TRUE, output_table = "calls_trimmed")
#'
#' # Trim last 10% of reads only for Alzheimers2, keep all other samples intact
#' trim_mod_reads(mod_db, by_frac = 0.1, trim_end = TRUE,
#'                samples = "Alzheimers2", output_table = "calls_trimmed_A2")
#'
#' # Trim both ends (5% each) for a subset of samples
#' trim_mod_reads(mod_db, by_frac = 0.05, trim_start = TRUE, trim_end = TRUE,
#'                samples = c("Control1", "Control2"), output_table = "calls_trimmed_controls")
#' }
#'
#' @importFrom DBI dbExecute dbListFields dbQuoteIdentifier dbQuoteString
#' @importFrom glue glue
#' @export
trim_mod_reads <- function(mod_db,
                           by_frac = 0.1,
                           trim_start = FALSE,
                           trim_end   = TRUE,
                           input_table  = "calls",
                           output_table = "calls_trimmed",
                           samples = NULL) {
  
  start_time <- Sys.time()
  
  # ---- sanity checks ----
  if (!is.numeric(by_frac) || length(by_frac) != 1 || is.na(by_frac)) {
    stop("by_frac must be a single numeric value.")
  }
  if (by_frac <= 0 || by_frac >= 1) {
    stop("by_frac must be between 0 and 1 (e.g. 0.1 for trimming last 10%).")
  }
  if (!is.logical(trim_start) || length(trim_start) != 1 ||
      !is.logical(trim_end)   || length(trim_end)   != 1) {
    stop("trim_start and trim_end must be single TRUE/FALSE values.")
  }
  if (!trim_start && !trim_end) {
    stop("Both trim_start and trim_end are set to FALSE — nothing to trim.")
  }
  
  mod_db <- ModSeqR:::.modhelper_connectDB(mod_db)
  
  # Helper for quoting identifiers
  qi <- function(x) as.character(DBI::dbQuoteIdentifier(.get_con(mod_db), x))
  
  # ---- Validate columns ----
  cols <- DBI::dbListFields(.get_con(mod_db), input_table)
  required <- c("read_position", "read_length")
  missing <- setdiff(required, cols)
  if (length(missing) > 0) {
    stop(glue::glue(
      "Input table '{input_table}' is missing required column(s): ",
      paste(missing, collapse = ", "), "."
    ))
  }
  
  # ---- Build trimming conditions (ONLY trimming; no sample filtering here) ----
  trim_conds <- character(0)
  
  if (trim_start) {
    trim_conds <- c(
      trim_conds,
      glue::glue("{qi('read_position')} >= {qi('read_length')} * {by_frac}")
    )
  }
  if (trim_end) {
    trim_conds <- c(
      trim_conds,
      glue::glue("{qi('read_position')} < {qi('read_length')} * (1 - {by_frac})")
    )
  }
  
  trim_clause <- paste(trim_conds, collapse = " AND ")
  
  # ---- Optional sample restriction: trim ONLY selected samples, keep others intact ----
  if (!is.null(samples)) {
    if (!"sample_name" %in% cols) {
      stop(glue::glue(
        "samples was provided, but input table '{input_table}' has no 'sample_name' column."
      ))
    }
    samples <- as.character(samples)
    if (length(samples) == 0) {
      stop("samples was provided but is empty. Use NULL to trim all samples.")
    }
    
    sample_sql <- paste(DBI::dbQuoteString(.get_con(mod_db), samples), collapse = ", ")
    
    # Keep all rows for non-target samples; trim only target samples
    where_clause <- glue::glue("
      (
        {qi('sample_name')} IN ({sample_sql})
        AND {trim_clause}
      )
      OR
      (
        {qi('sample_name')} NOT IN ({sample_sql})
      )
    ")
  } else {
    # Trim all samples
    where_clause <- trim_clause
  }
  
  # ---- Execute ----
  query <- glue::glue("
    CREATE OR REPLACE TABLE {qi(output_table)} AS
    SELECT *
    FROM {qi(input_table)}
    WHERE {where_clause}
  ")
  
  DBI::dbExecute(.get_con(mod_db), query)
  
  elapsed <- Sys.time() - start_time
  message(sprintf(
    "trim_mod_reads: wrote '%s' from '%s' (by_frac=%.3f, start=%s, end=%s, samples=%s) in %.2f sec",
    output_table, input_table, by_frac, trim_start, trim_end,
    if (is.null(samples)) "ALL" else paste(samples, collapse = ","),
    as.numeric(elapsed, units = "secs")
  ))
  
  mod_db$last_result <- dplyr::tbl(.get_con(mod_db), output_table) |>
    dplyr::count(sample_name) |>
    dplyr::collect()
  mod_db$current_table <- output_table
  invisible(mod_db)
}
