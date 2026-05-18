#' ModSeqR: Methylation Data Analysis from Nanopore Sequencing
#'
#' ModSeqR is an R package developed by Wasatch Biolabs for efficient
#' preprocessing, summarization, and visualization of methylation data.
#' It supports native DNA methylation sequencing workflows and is
#' optimized for high-throughput pipelines.
#'
#' @section Features:
#' - Database-backed storage using DuckDB
#' - Sliding window, positional, regional, and read-level summarization of methylation levels
#' - Support for multiple methylation contexts
#' - Quality Control Visualization
#' - Efficient Differential Methylation Analysis
#'
#' @section The mod_db object:
#' Most ModSeqR functions accept and return an object of class \code{"mod_db"}, a named
#' list with four slots:
#' \describe{
#'   \item{\code{db_file}}{Character. Path to the \code{.mod.db} DuckDB file on disk.}
#'   \item{\code{current_table}}{Character or \code{NULL}. Name of the most recently
#'     written or active table; updated automatically by pipeline functions.}
#'   \item{\code{con}}{A DBI connection object while a function is running;
#'     \code{"none"} when the connection has been closed.}
#'   \item{\code{last_result}}{The most recently computed in-memory result (e.g., a
#'     correlation matrix, a PCA list). \code{NULL} if no result has been stored yet.
#'     Retrieve with \code{get_mod_result(mod_db)} or \code{mod_db$last_result}.}
#' }
#'
#' Printing a \code{mod_db} object (e.g., \code{print(mod_db)} or just typing the
#' variable name) shows all four slots:
#'
#' \preformatted{
#' <mod_db object>
#' Database file:
#'   my_data.mod.db
#' Current table:
#'   mod_diff_windows
#' Connection:
#'   NULL
#' Last result: <matrix> [6 x 6]
#' }
#'
#' Because most functions return \code{invisible(mod_db)}, they can be chained with
#' the native pipe:
#'
#' \preformatted{
#' mod_db <- make_mod_db(ch3_files, "my_db") |>
#'   summarize_mod_windows()              |>
#'   calc_mod_diff(cases = ..., controls = ...)
#' }
#'
#' Functions that compute an in-memory result (\code{calc_mod_samplecor},
#' \code{plot_mod_pca}, \code{calc_mod_diff}) store it in \code{last_result} so it
#' can be retrieved after the pipe without breaking the chain:
#'
#' \preformatted{
#' mod_db    <- calc_mod_samplecor(mod_db)
#' cor_mat   <- get_mod_result(mod_db)   # numeric correlation matrix
#'
#' mod_db    <- plot_mod_pca(mod_db)
#' pca_list  <- get_mod_result(mod_db)   # list(pca = <prcomp>, scores = <data frame>)
#' }
#'
#' @author
#' Jonathon Hill \email{jonathon@wasatchbiolabs.com} (aut, cre)
#' Hailey Zimmerman \email{hailey@renewbt.com} (aut)
#' 
#' @name ModSeqR
#'
#' @references
#' Wasatch Biolabs (2025). *ModSeqR: Tools for Methylation Analysis in Clinical and Research Settings.*
#' https://www.wasatchbiolabs.com/
#' 
#' For bug reports and feature requests:  
#' https://github.com/Wasatch-Biolabs-Bfx/ModSeqR
"_PACKAGE"

.onAttach <- function(lib, pkg)
{
  pv <- packageVersion(pkg)
  msg1 <- 
    glue::glue("========================================",  
               "    ╔╦╗╔═╗╔╦╗╔═╗╔═╗╔═╗ ╦═╗                ",
               "    ║║║║ ║ ║║╚═╗║╣ ║═╬╗╠╦╝               ",
               "    ╩ ╩╚═╝═╩╝╚═╝╚═╝╚═╝╚╩╚═{pv}              ",
               "========================================",
               .sep = "\n")
  
  msg2 <- paste("ModSeqR v", packageVersion(pkg))
  
  if (interactive()) {
    packageStartupMessage(msg1)
  } else {
    packageStartupMessage(msg2)
  }
  
  packageStartupMessage("Created by Wasatch Biolabs")
  packageStartupMessage("Research & Clinical Nanopore Sequencing")
  packageStartupMessage("www.wasatchbiolabs.com")
  packageStartupMessage("")
  packageStartupMessage("This package is licensed for personal") 
  packageStartupMessage("and internal research use only.")
  packageStartupMessage("See the LICENSE file or visit")
  packageStartupMessage("https://github.com/Wasatch-Biolabs-Bfx/ModSeqR/blob/main/LICENSE.")
}
