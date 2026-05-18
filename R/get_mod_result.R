#' Retrieve the Last Computed Result from a mod_db Object
#'
#' Returns the result object stored by the most recent pipe-compatible function
#' (e.g., the correlation matrix from \code{calc_mod_samplecor()}, or the PCA
#' list from \code{plot_mod_pca()}). Returns \code{NULL} if no result has been
#' stored yet.
#'
#' @param mod_db A \code{"mod_db"} object.
#'
#' @return The stored result object, or \code{NULL}. The type depends on which
#'   function populated it:
#'   \itemize{
#'     \item \code{calc_mod_samplecor()} — a numeric correlation \code{matrix}
#'     \item \code{plot_mod_pca()} — a named \code{list} with elements
#'       \code{pca} (\code{prcomp} object) and \code{scores} (data frame of PC scores)
#'     \item \code{calc_mod_diff()} — a data frame preview of the result table
#'   }
#'
#' @examples
#' \dontrun{
#' mod_db <- calc_mod_samplecor(mod_db)
#' cor_matrix <- get_mod_result(mod_db)
#'
#' mod_db <- plot_mod_pca(mod_db)
#' pca_scores <- get_mod_result(mod_db)$scores
#' }
#'
#' @export
get_mod_result <- function(mod_db) {
  mod_db$last_result
}
