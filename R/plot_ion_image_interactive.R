#' Plot an interactive ion image of a target m/z using ggplot2 and plotly
#'
#' @param mse An MSImagingExperiment object
#' @param target_mz Numeric, the target m/z to visualize
#' @param tol Numeric, tolerance for visualization around target m/z
#' @param units Character, defines units for tolerance `tol` either "mz" or "ppm"
#' @param flip_y Logical, Whether the y-axis should be flipped to match normal Cardinal plotting
#' @return A ggplot object
#' @export
plot_ion_image_interactive <- function(mse, target_mz, tol = 0.1, units = c("mz", "ppm"),
                                       palette = c("viridis", "magma", "inferno", "plasma",
                                                   "cividis", "rocket", "mako", "turbo"),
                                       bg = c("white", "black"), flip_y = TRUE) {

  units <- match.arg(units)
  palette <- match.arg(palette)
  bg <- match.arg(bg)

  mse_name <- deparse(substitute(mse))
  text_color <- if (bg == "black") "white" else "black"

  # --- feature matching ---
  tol_mz <- if (units == "ppm") target_mz * tol * 1e-6 else tol
  lo_mz <- target_mz - tol_mz
  hi_mz <- target_mz + tol_mz

  fid <- Cardinal::features(mse, lo_mz < mz, mz < hi_mz)
  if (length(fid) == 0) {
    stop("No features found within tolerance of target_mz = ", target_mz)
  }

  ion_intensity <- if (length(fid) == 1) {
    Cardinal::spectra(mse)[fid, ]
  } else {
    colSums(Cardinal::spectra(mse)[fid, , drop = FALSE])
  }

  df <- data.frame(Cardinal::coord(mse), intensity = ion_intensity)

  # --- color palette ---
  pal_colors <- viridisLite::viridis(256, option = palette)

  # --- plot ---
  plotly::plot_ly(
    df, x = ~x, y = ~y, z = ~intensity,
    type = "heatmap",
    colors = pal_colors,
    hovertemplate = "x: %{x}<br>y: %{y}<br>Intensity: %{z:.1f}<extra></extra>"
  ) |>
    plotly::layout(
      title = list(
        text = paste0(mse_name, " - ", round(target_mz, 4), " <i>m/z</i> ",
                      "\u00B1", tol, if (units == "ppm") " ppm" else " Da"),
        font = list(color = text_color)
      ),
      paper_bgcolor = bg,
      plot_bgcolor = bg,
      font = list(color = text_color),
      xaxis = list(title = "x", showgrid = FALSE, zeroline = FALSE,
                   range = range(df$x), color = text_color),
      yaxis = list(title = "y", showgrid = FALSE, zeroline = FALSE, autorange = if (flip_y) "reversed" else TRUE,
                   range = range(df$y), color = text_color)
    ) |>
    plotly::colorbar(title = "Intensity",
                     tickfont = list(color = text_color),
                     titlefont = list(color = text_color))
}
