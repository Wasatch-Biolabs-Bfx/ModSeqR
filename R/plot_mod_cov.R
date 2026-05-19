#' Calculate and Plot Coverage Statistics
#'
#' This function calculates and optionally plots statistics for coverage data from
#' methylation sequencing experiments. It can handle both positional and regional
#' methylation data.
#'
#' @param mod_db A data base either linking to the file name or of class mod_db.
#' @param input_table A string specifying the table name to analyze coverage on.
#'   Default is \code{"positions"}.
#' @param plot Logical, if \code{TRUE}, the function will generate a histogram of
#' the coverage data. Default is \code{FALSE}.
#' @param save_path Pathway to save the plot to. Usually .pdf or .png.
#' @param max_rows The maximum amount of rows wanted for calculation. This argument can help analysis run faster when there is a lot of data.
#' @param call_type Deprecated. Use \code{input_table} instead.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return). Prints
#'   summary statistics when \code{plot = FALSE}, or a histogram when \code{plot = TRUE}.
#'   \code{last_result} is set to the \code{ggplot} object when \code{plot = TRUE}.
#'
#' @examples
#'  # Specify the path to the database
#'  mod_db <- system.file("my_data.mod.db", package = "ModSeqR")
#'
#'  # Get coverage statistics for the 'positions' input table without plotting
#'  plot_mod_cov(mod_db = mod_db, input_table = "positions")
#'
#' @importFrom DBI dbConnect dbDisconnect dbExistsTable
#' @importFrom duckdb duckdb
#' @importFrom dplyr tbl summarise pull sql
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal ggsave
#' @importFrom stats quantile na.omit
#'
#' @export

plot_mod_cov <- function(mod_db,
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
  if (!dbExistsTable(mod_db$con, input_table)) {
    stop(paste0(input_table, " Table does not exist in the database. Check spelling or make sure you create it first.\n"))
  }

  # Determine total number of rows first
  total_rows <- tbl(mod_db$con, input_table) |> summarise(n = n()) |> pull(n)

  # Sample in SQL if max_rows is given and valid
  if (!is.null(max_rows)) {
    if (max_rows > total_rows) {
      stop(paste0("Requested max_rows (", max_rows,
                  ") exceeds available rows in the table (", total_rows, ")."))
    }

    modseq_dat <- tbl(mod_db$con, sql(paste0(
      "SELECT * FROM ", input_table,
      " USING SAMPLE ", max_rows, " ROWS"
    )))
  } else {
    modseq_dat <- tbl(mod_db$con, input_table)
  }

  # Checks
  stopifnot("Invalid dataframe format. A 'num_calls' or 'mean_num_calls' column must be present." =
              any(c("num_calls", "mean_num_calls") %in% colnames(modseq_dat)))

  # Clean dataframe
  modseq_dat <- na.omit(modseq_dat)

  # Decide if per base or per region
  regional_dat = "region_name" %in% colnames(modseq_dat)
  windows_dat <- "num_sites" %in% colnames(modseq_dat) && !regional_dat

  # if (!regional_dat) {
  num_calls = pull(modseq_dat, num_calls)
  # } else {
  #   num_calls = pull(modseq_dat, mean_num_calls)
  # }

  qts <- c(seq(0, 0.9, 0.1), 0.95, 0.99, 0.995, 0.999, 1)

  # PLOT COVERAGE STATS
  title <- "read coverage statistics per base\n"

  if (regional_dat) {
    title <- "read coverage statistics per region\n"
  } else if (windows_dat) {
    title <- "read coverage statistics per window\n"
  }

  cat(title)
  cat("summary:\n")
  print( summary( num_calls ) )
  cat("percentiles:\n")
  print(quantile(num_calls, p=qts ))
  cat("\n")

  if (plot) {
    x_title <- "log10 of read coverage per base"
    if (regional_dat) {
      x_title <- "log10 of read coverage per region"
    } else if (windows_dat) {
      x_title <- "log10 of read coverage per window"
    }

    # Create a data frame from your list
    plot <- data.frame(coverage = log10(num_calls))

    # Create the histogram
    p <- ggplot(plot, aes(x = coverage)) +
      geom_histogram(
        binwidth = 0.25,
        fill = "chartreuse4",
        color = "black",
        linewidth = 0.25) +
      labs(
        title = "Histogram of CpG Coverage",
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
      cat("Coverage plot saved to ", save_path, "\n")
    }
  }

  end_time <- Sys.time()
  message("Time elapsed: ", end_time - start_time, "\n")

  mod_db <- .modhelper_closeDB(mod_db)
  invisible(mod_db)
}
