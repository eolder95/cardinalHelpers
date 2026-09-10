#' Plot an interactive spectrum from a MSImagingExperiment (mse) object using plotly
#'
#' @param mse An MSImagingExperiment object
#' @param pixel Numeric, default `NULL` will automatically select the pixel with the highest sum TIC, otherwise use to select a pixel for plotting
#' @param mz_range Numeric, default `NULL` plots the whole mass range. Set a range using `c(<lo_mz>, <hi_mz>)`
#' @param max_points Integer, maximum number of points to plot, helps speed up plotting but can result in skewed peak shape when zooming in
#' @return A plotly object
#' @export
plot_mse_spectrum_interactive <- function(mse, pixel = NULL, mz_range = NULL,
                                          max_points = 8000) {

  mse_name <- deparse(substitute(mse))

  # --- 1. pick pixel with highest total ion count, if none specified ---
  if (is.null(pixel)) {
    pixel <- which.max(colSums(spectra(mse)))
  }

  mzs <- Cardinal::mz(mse)
  ints <- Cardinal::spectra(mse)[, pixel]

  # --- 2. optional mz range restriction ---
  if (!is.null(mz_range)) {
    keep <- mzs >= mz_range[1] & mzs <= mz_range[2]
    mzs <- mzs[keep]
    ints <- ints[keep]
  }

  # --- 3. peak-preserving downsample if large ---
  if (length(mzs) > max_points) {
    n_bins <- max_points
    bins <- cut(seq_along(mzs), breaks = n_bins, labels = FALSE)
    idx <- unlist(tapply(seq_along(mzs), bins, function(i) i[which.max(ints[i])]))
    idx <- sort(idx)
    mzs <- mzs[idx]
    ints <- ints[idx]
  }

  df <- data.frame(mz = mzs, intensity = ints)

  # --- 4. plot ---
  p <- plotly::plot_ly(
    df, x = ~mz, y = ~intensity,
    type = "scattergl", mode = "lines",
    line = list(width = 1, color = "black"),
    hovertemplate = "m/z: %{x:.4f}<br>Intensity: %{y:.1f}<extra></extra>"
  ) |>
    plotly::layout(
      title = paste0(mse_name, " - spectrum ", pixel,
                     " (highest total ion count)"),
      xaxis = list(title = "<i>m/z</i>"),
      yaxis = list(title = "Intensity"),
      hovermode = "closest"
    )

  return(p)
}
