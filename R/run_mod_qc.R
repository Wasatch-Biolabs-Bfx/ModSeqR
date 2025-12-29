#' Quality Control Wrapper for Methylation Data
#'
#' This function serves as a wrapper for various quality control analyses on methylation data.
#' It sequentially calculates coverage statistics, modification statistics, correlation analysis,
#' and performs principal component analysis (PCA).
#'
#' @param mod_db A database connection or object containing methylation data.
#' @param call_type A character string indicating the type of call to retrieve data (e.g., "positions", "regions").
#' @param plot Logical; whether to generate plots. Defaults to TRUE.
#' @param max_rows Optional maximum number of rows to use (sampling) for speed on large datasets.
#' @param value Column to use for analyses that take a value input (e.g., correlations/PCA).
#'   Accepts a bare column name or a string. Default: m_frac.
#'
#' @return Invisibly returns nothing; produces plots and console output.
#'
#' @examples 
#' \dontrun{
#'  mod_db <- system.file("my_data.mod.db", package = "MethylSeqR")
#'  run_mod_qc(mod_db, call_type = "positions")                 # uses m_frac
#'  run_mod_qc(mod_db, call_type = "regions", value = mh_frac)  # use mh_frac
#' }
#'
#' @importFrom rlang ensym as_name
#' @export
run_mod_qc <- function(mod_db, 
                       call_type = "positions", 
                       plot = TRUE, 
                       max_rows = NULL,
                       value = m_frac) {
  
  value_sym <- rlang::ensym(value)
  value_col <- rlang::as_name(value_sym)  # e.g., "m_frac"
  
  # accepted_tables <- c("calls","positions","regions","windows")
  # if (!(call_type %in% accepted_tables)) {
  #   stop("Quality control plots can currently only be made for calls, positions, regions, or windows.")
  # }
  
  cat(paste0("Running quality control on ", call_type, " table.\n"))
  start_time <- Sys.time()
  
  if (call_type == "calls") {
    suppressMessages(suppressWarnings(summarize_mod_positions(mod_db)))
    call_type <- "positions"
  }
  
  message("calculating coverage stats...")
  plot_mod_cov(mod_db, call_type, plot = plot, max_rows = max_rows)
  
  message("calculating mod stats...")
  plot_mod_modfrac(mod_db, call_type, plot = plot, max_rows = max_rows)
  
  message("calculating correlations...")
  do.call(calc_mod_samplecor, list(
    mod_db   = mod_db,
    call_type = call_type,
    value    = value_sym,   # <-- literal string now for argument parameter
    plot     = plot,
    max_rows = max_rows
  ))
  
  message("running pca...")
  do.call(plot_mod_pca, list(
    mod_db   = mod_db,
    call_type= call_type,
    value    = value_sym,   # <-- literal string now for argument parameter
    max_rows = max_rows
  ))
  
  end_time <- Sys.time()
  secs <- as.numeric(end_time - start_time, units = "secs")
  if (secs > 60) {
    mins <- as.numeric(end_time - start_time, units = "mins")
    message("QC complete!\nTime elapsed: ", round(mins, 2), " minutes\n")
  } else {
    message("QC complete!\nTime elapsed: ", round(secs, 2), " seconds\n")
  }
  invisible(NULL)
}
