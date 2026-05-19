#' Plot Case vs Control Methylation Fractions as a Density Heatmap
#'
#' Generates a 2-D density heatmap of `mh_frac_case` vs `mh_frac_control` methylation
#' fractions from a differential-methylation table in a DuckDB database.
#' Density is estimated with \code{MASS::kde2d} and rendered as a raster layer.
#' Optional threshold lines can be drawn above and below the identity line.
#'
#' @param mod_db A path to the modification DuckDB database, or an open database connection object.
#' @param table A string specifying the table name within the database, containing
#'   \code{mh_frac_case} and \code{mh_frac_control} columns.
#' @param thresh A numeric vector of length 1 or 2 giving offset distances from the
#'   identity line at which to draw dashed red threshold lines (default \code{c(0.2, 0.3)}).
#'   For each value \code{t}, lines are drawn at \code{y = x + t} and \code{y = x - t}.
#'   If \code{NULL}, no threshold lines are drawn.
#' @param palette A string specifying the viridis color palette for density shading
#'   (default \code{"turbo"}). Other options include \code{"viridis"}, \code{"plasma"},
#'   \code{"cividis"}, etc.
#' @param n_kde Integer. Grid resolution passed to \code{MASS::kde2d} (default \code{250}).
#'   Higher values produce a smoother surface at the cost of speed.
#'
#' @return Invisibly returns the \code{"mod_db"} object (connection closed on return). The plot
#'   is printed to the current graphics device. \code{last_result} is set to the \code{ggplot}
#'   object.
#'
#' @details
#' The function pulls \code{mh_frac_case} and \code{mh_frac_control} from \code{table},
#' estimates a 2-D kernel density with \code{MASS::kde2d} on the unit square \code{[0,1]²},
#' and renders the surface using \code{geom_raster}. A solid black identity line
#' (\code{y = x}) is overlaid for reference. If \code{thresh} is non-\code{NULL},
#' dashed red lines are added at \code{y = x ± t} for each value \code{t} in
#' \code{thresh}. Both axes are constrained to \code{[0, 1]}.
#'
#' @examples
#' \dontrun{
#' plot_mod_diff_density("my_data/my.mod.db", table = "chr5", thresh = c(0.25, 0.35))
#' }
#'
#' @importFrom DBI dbExistsTable
#' @importFrom dplyr tbl select filter collect
#' @importFrom ggplot2 ggplot aes geom_abline geom_raster coord_equal labs theme_minimal
#' @importFrom ggplot2 scale_fill_viridis_c
#' @importFrom MASS kde2d
#' @export

plot_mod_diff_density <- function(mod_db,
                                  table,
                                  thresh   = c(0.2, 0.3),
                                  palette  = "turbo",
                                  n_kde    = 250)
{
  
  mod_db <- .modhelper_connectDB(mod_db)
  on.exit(mod_db <- .modhelper_closeDB(mod_db), add = TRUE)
  
  if (!DBI::dbExistsTable(mod_db$con, table))
    stop(table, " table does not exist.")
  
  df <- dplyr::tbl(mod_db$con, table) |>
    dplyr::select(mh_frac_case, mh_frac_control) |>
    dplyr::filter(!is.na(mh_frac_case), !is.na(mh_frac_control)) |>
    dplyr::collect()
  
  if (nrow(df) < 20) stop("Too few points to draw a density surface.")
  
  # ── KDE surface  ---------------------------------------------------------
  kd <- MASS::kde2d(df$mh_frac_case, df$mh_frac_control,
                    n    = n_kde,
                    lims = c(0, 1, 0, 1))
  
  surf <- expand.grid(x = kd$x, y = kd$y)
  surf$z <- as.vector(kd$z)
  
  # ── base plot  -----------------------------------------------------------
  p <- ggplot2::ggplot(surf, ggplot2::aes(x, y, fill = z)) +
    ggplot2::geom_raster(interpolate = TRUE) +
    ggplot2::scale_fill_viridis_c(
      option = palette, name = "Density", guide = "colourbar"
    ) +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "black")

  if (!is.null(thresh)) {
    for (t in thresh) {
      p <- p +
        ggplot2::geom_abline(slope = 1, intercept =  t, colour = "red", linetype = "dashed") +
        ggplot2::geom_abline(slope = 1, intercept = -t, colour = "red", linetype = "dashed")
    }
  }

  p <- p +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1), expand = FALSE) +
    ggplot2::labs(
      title = "Case vs Control Mod Fractions",
      x = "mh_frac_case",
      y = "mh_frac_control"
    ) +
    ggplot2::theme_minimal()

  print(p)
  mod_db$last_result <- p
  invisible(mod_db)
}
