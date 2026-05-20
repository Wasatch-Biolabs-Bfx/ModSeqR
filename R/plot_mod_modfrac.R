#' Get Methylation Statistics from the Modifications Database
#'
#' This function retrieves and calculates methylation statistics (mean methylation fractions)
#' from a specified table in the mod database. It can either return summary statistics or plot
#' a histogram of the methylation values, depending on the user's preference.
#'
#' @param mod_db A string. The path to the database containing mod files from nanopore data.
#' @param input_table A string specifying the table name to retrieve data from.
#'   Default is \code{"positions"}.
#' @param plot A logical value. If TRUE, a histogram of methylation values is plotted. Default is FALSE.
#' @param save_path Pathway to save the plot to. Usually .pdf or .png.
#' @param max_rows The maximum amount of rows wanted for calculation. This argument can help analysis run faster when there is a lot of data.
#' @param call_type Deprecated. Use \code{input_table} instead.
#'
#' @details
#' The function connects to the specified database, checks for the existence of the relevant table,
#' and retrieves methylation fraction data. If the table contains methylation data, it prioritizes
#' the `mh_frac` column over others. Depending on the input table type, it can compute statistics
#' for either per base, per window, or per region. If `plot` is set to TRUE, it generates a histogram of the
#' methylation values.
#'
#' @note The function auto-detects the table type from its columns to determine the appropriate
#' axis labels and title.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return). Prints
#'   summary statistics when \code{plot = FALSE}, or a histogram when \code{plot = TRUE}.
#'   \code{last_result} is set to the \code{ggplot} object when \code{plot = TRUE}.
#'
#' @examples
#'  # Specify the path to the database
#'  mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
#'
#'  # Get methylation statistics for the 'positions' input table without plotting
#'  plot_mod_modfrac(mod_db = mod_db, input_table = "positions")
#'
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl summarise pull sql
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal ggsave
#' @importFrom stats quantile
#'
#' @export

plot_mod_modfrac <- function(mod_db,
                             input_table = "positions",
                             plot = TRUE,
                             save_path = NULL,
                             max_rows = NULL,
                             call_type = NULL)
{
  if (!is.null(call_type)) {
    warning("'call_type' is deprecated; use 'input_table' instead.", call. = FALSE)
    input_table <- call_type
  }

  start_time <- Sys.time()
  # Open the database connection
  mod_db <- .modhelper_connectDB(mod_db)

  # Check for specific table and connect to it in the database
  if (!dbExistsTable(.get_con(mod_db), input_table)) {
    stop(paste0(input_table, " Table does not exist in the database. Check spelling or make sure you create it first.\n"))
  }

  # Determine total number of rows first
  total_rows <- tbl(.get_con(mod_db), input_table) |> summarise(n = n()) |> pull(n)

  # Sample in SQL if max_rows is given and valid
  if (!is.null(max_rows)) {
    if (max_rows > total_rows) {
      stop(paste0("Requested max_rows (", max_rows,
                  ") exceeds available rows in the table (", total_rows, ")."))
    }

    modseq_dat <- tbl(.get_con(mod_db), sql(paste0(
      "SELECT * FROM ", input_table,
      " USING SAMPLE ", max_rows, " ROWS"
    )))
  } else {
    modseq_dat <- tbl(.get_con(mod_db), input_table)
  }

  # decide if per base or per region
  regional_dat = "region_name" %in% colnames(modseq_dat)
  windows_dat <- "num_sites" %in% colnames(modseq_dat) && !regional_dat

  # grab mh frac info- prioritize mh_frac over m_frac
  if ("mh_frac" %in% colnames(modseq_dat)) {
    goodMeth = 100 * pull(modseq_dat, mh_frac)
  } else if ("m_frac" %in% colnames(modseq_dat)) {
    goodMeth = 100 * pull(modseq_dat, m_frac)
  }

  qts <- c(seq(0, 0.9, 0.1), 0.95, 0.99, 0.995, 0.999, 1)

  title <- "Methylation statistics per base\n"
  if (regional_dat) {
    title <- "Methylation statistics per region\n"
  } else if (windows_dat) {
    title <- "Methylation statistics per window\n"
  }

  cat(title)
  cat("Summary:\n")
  print(summary(goodMeth, p=qts))
  cat("percentiles:\n")
  print(quantile(goodMeth, p=qts))
  cat("\n")


  if (plot) { # if they want a plot
    x_title <- "% methylation per base"
    if (regional_dat) {
      x_title <- "% methylation per region"
    } else if (windows_dat) {
      x_title <- "% methylation per window"
    }

    # Create a data frame from your list
    plot <- data.frame(methylation_value = goodMeth)

    # Create the histogram
    p <- ggplot(plot, aes(x = methylation_value)) +
      geom_histogram(
        binwidth = 10,
        fill = "cornflowerblue",
        color = "black",
        linewidth = 0.25) +
      labs(
        title = "Histogram of % CpG Methylation",
        x = x_title,
        y = "Frequency") +
      theme_minimal() +
      theme(
        #panel.grid = element_blank(),              # removes all gridlines
        plot.title = element_text(hjust = 0.5)     # centers the plot title
      )

    print(p)
    mod_db$last_result <- p

    # Save the plot if save_path is specified
    if (!is.null(save_path)) {
      ggsave(filename = save_path, plot = p, width = 8, height = 6, dpi = 300)
      cat("Statistics plot saved to ", save_path, "\n")
    }
  }

  end_time <- Sys.time()

  message("Time elapsed: ", end_time - start_time, "\n")
  invisible(mod_db)
}
