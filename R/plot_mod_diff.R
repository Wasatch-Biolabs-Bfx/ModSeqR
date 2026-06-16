#' Plot Differential Methylation Volcano Raster
#'
#' Creates a volcano-style raster plot of differential methylation results using database-backed plotting via `dbplot`.
#' This function connects to a DuckDB database, retrieves the specified differential methylation table, filters and
#' transforms the data, and generates a raster plot with `meth_diff` on the x-axis and `-log10(p-value)` on the y-axis.
#'
#' @param mod_db A `mod_db` object or a character string representing the file path to a DuckDB database.
#'        The database must contain a table with differential methylation results.
#' @param table_name A character string specifying the name of the table in the database containing
#'        the differential methylation data. Required columns: \code{meth_diff} and \code{p_val}.
#' @param table Deprecated. Use \code{table_name} instead.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return). The plot
#'   is printed to the active graphics device. \code{last_result} is set to the \code{ggplot}
#'   object.
#'
#' @details
#' The plot uses `dbplot::dbplot_raster()` to efficiently create a raster visualization of large-scale methylation difference data.
#' It applies a log10 transformation to the `p_val` column and uses a color gradient to show the density of observations in each bin.
#' A message is printed to indicate the time taken to generate the plot.
#'
#' @examples
#' \dontrun{
#' plot_mod_diff("my_methylation.mod.db", "mod_diff_windows")
#' }
#'
#' @param fdr_cutoff Numeric. FDR (BH-adjusted p) threshold used to mark genome-wide
#'   significance on the plot. When the table has a \code{p_adjust} column, the plot adds a
#'   dashed line at the raw-p level corresponding to this FDR cutoff (the largest \code{p_val}
#'   among windows passing it) and a subtitle stating how many windows are significant. This
#'   prevents the raw-p volcano from implying signal that does not survive multiple-testing
#'   correction. Default \code{0.05}.
#'
#' @importFrom dplyr filter mutate
#' @importFrom dbplot dbplot_raster
#' @importFrom ggplot2 scale_fill_viridis_c labs theme_minimal geom_hline annotate aes
#' @importFrom DBI dbExistsTable dbListFields dbGetQuery
#' @export
plot_mod_diff <- function(mod_db,
                          table_name,
                          fdr_cutoff = 0.05,
                          table = NULL) {
  if (!is.null(table)) {
    warning("'table' is deprecated; use 'table_name' instead.", call. = FALSE)
    table_name <- table
  }

  start_time <- Sys.time()
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)

  # check for differential methylation table
  if (!dbExistsTable(.get_con(mod_db), table_name)) {
    stop(paste0(table_name, " table does not exist. Build it with calc_mod_diff()!"))
  }

  # Check required columns
  .modhelper_check_cols(.get_con(mod_db), table_name, c("meth_diff", "p_val"))

  # Connect to the table
  tbl_diff <- tbl(.get_con(mod_db), table_name)

  # FDR-significance summary: the volcano axis is RAW -log10(p), which always looks
  # "significant" because ~fdr_cutoff of windows clear raw p by chance. If p_adjust is
  # available, compute how many windows survive BH-FDR and the raw-p line corresponding to
  # that cutoff, so the plot honestly shows the genome-wide-significant threshold.
  con      <- .get_con(mod_db)
  has_padj <- "p_adjust" %in% DBI::dbListFields(con, table_name)
  sig_line <- NA_real_; sub <- NULL
  if (has_padj) {
    s <- DBI::dbGetQuery(con, sprintf(
      "SELECT COUNT(*) FILTER (WHERE p_adjust < %1$g) AS n_sig,
              MAX(p_val) FILTER (WHERE p_adjust < %1$g) AS crit_p
       FROM %2$s WHERE p_val > 0 AND p_val IS NOT NULL", fdr_cutoff, table_name))
    sub <- sprintf("%s of the tested windows significant at FDR < %g",
                   format(s$n_sig, big.mark = ","), fdr_cutoff)
    if (!is.na(s$crit_p) && s$crit_p > 0) sig_line <- -log10(s$crit_p)
  }

  # Plot using dbplot_raster
  plot <- tbl(.get_con(mod_db), table_name) |>
    filter(
      !is.na(meth_diff),
      !is.nan(meth_diff),
      !is.na(p_val),
      p_val > 0
    ) |>
    mutate(log_p = -log10(p_val)) |>
    dbplot::dbplot_raster(meth_diff, log_p)

  plot <- plot +
    scale_fill_viridis_c(option = "rocket", direction = -1, begin = 0.2, end = 0.9) +  # Fancy color gradient- add option = "mako"
    labs(
      title    = "Differential Methylation Volcano Plot",
      subtitle = sub,
      x = "Methylation Difference (case-control)",
      y = "-log10(p-value)",
      fill = "Count"
    ) +
    theme_minimal()

  # Genome-wide significance line (raw-p level meeting the FDR cutoff), if any window passes.
  if (!is.na(sig_line)) {
    plot <- plot +
      geom_hline(yintercept = sig_line, linetype = "dashed", colour = "red") +
      annotate("text", x = Inf, y = sig_line, hjust = 1.05, vjust = -0.5,
               label = sprintf("FDR < %g", fdr_cutoff), colour = "red", size = 3.5)
  }

  print(plot)

  end_time <- Sys.time()

  total_time_difftime <- end_time - start_time

  # Convert the total_time_difftime object to numeric seconds for a reliable comparison
  total_seconds <- as.numeric(total_time_difftime, units = "secs")

  if (total_seconds > 60) {
    # If greater than 60 seconds, convert to numeric minutes for display
    total_minutes <- as.numeric(total_time_difftime, units = "mins")
    message("Differential Methylation Plotted!",
            "\nTime elapsed: ", round(total_minutes, 2), " minutes\n")
  } else {
    # Otherwise, display in numeric seconds
    message("Differential Methylation Plotted!",
            "\nTime elapsed: ", round(total_seconds, 2), " seconds\n")
  }
  mod_db$last_result <- plot
  invisible(mod_db)
}
