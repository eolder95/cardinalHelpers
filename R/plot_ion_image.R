#' Plot an ion image of a target m/z using ggplot2
#'
#' @param mse An MSImagingExperiment object
#' @param target_mz Numeric, the target m/z to visualize
#' @param tol Numeric, tolerance for visualization around target m/z
#' @param units Character, defines units for tolerance `tol` either "mz" or "ppm"
#' @param flip_y Boolean, Whether the y-axis should be flipped to match normal Cardinal plotting
#' @return A ggplot object
#' @export
plot_ion_image <- function(mse, target_mz, tol = 0.1, units = c("mz", "ppm"),
                           flip_y = TRUE) {

  units <- match.arg(units)

  ## capture object name for title, before any manipulation
  mse_name <- deparse(substitute(mse))

  ## Find feature indices within tolerance of target_mz
  if (units == "ppm") {
    tol_mz <- target_mz * tol * 1e-6
  } else {
    tol_mz <- tol
  }

  fid <- cardinal::features(mse, target_mz - tol_mz < mz, mz < target_mz + tol_mz)

  if (length(fid) == 0) {
    stop("No features found within tolerance of target_mz = ", target_mz)
  }

  ## Sum intensity across matched features per pixel
  if (length(fid) == 1) {
    ion_intensity <- cardinal::spectra(mse)[fid, ]
  } else {
    ion_intensity <- colSums(cardinal::spectra(mse)[fid, , drop = FALSE])
  }

  ## Build data frame with coordinates
  df <- data.frame(
    cardinal::coord(mse),
    intensity = ion_intensity
  )

  ## Plot
  p <- ggplot2::ggplot(df, aes(x = x, y = y, fill = intensity)) +
    ggplot2::geom_raster() +
    ggplot2::scale_fill_viridis_c() +
    ggplot2::coord_fixed() +
    ggplot2::labs(
      title = paste0(mse_name, " — m/z ", round(target_mz, 3),
                     " (\u00B1", tol, if (units == "ppm") " ppm" else "", ")"),
      fill = "Intensity", x = "x", y = "y"
    ) +
    ggplot2::theme_minimal()

  if (flip_y) {
    p <- p + ggplot2::scale_y_reverse()
  }

  return(p)
}
