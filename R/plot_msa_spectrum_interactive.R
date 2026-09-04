#' Plot an interactive spectrum from a MSImagingArrays (msa) object using plotly
#'
#' @param mse An MSImagingArrays object
#' @param pixel Numeric, default `NULL` will automatically select the pixel with the highest sum TIC, otherwise use to select a pixel for plotting
#' @param mz_range Numeric, default `NULL` plots the whole mass range. Set a range using `c(<lo_mz>, <hi_mz>)`
#' @param max_points Integer, maximum number of points to plot, helps speed up plotting but can result in skewed peak shape when zooming in
#' @return A plotly object
#' @export
plot_msa_spectrum_interactive <- function(msa, pixel = NULL, mz_range = NULL,
                                          max_points = 50000) {

  msa_name <- deparse(substitute(msa))

  mz_list <- Cardinal::mz(msa)
  int_list <- Cardinal::intensity(msa)

  ## Pick a spectrum if none specified: highest total ion count
  if (is.null(pixel)) {
    total_ion <- sapply(int_list, sum)
    pixel <- which.max(total_ion)
  }

  mzs <- mz_list[[pixel]]
  ints <- int_list[[pixel]]

  ## Optional mz range restriction
  if (!is.null(mz_range)) {
    keep <- mzs >= mz_range[1] & mzs <= mz_range[2]
    mzs <- mzs[keep]
    ints <- ints[keep]
  }

  ## Peak-preserving downsample if large
  if (length(mzs) > max_points) {
    n_bins <- max_points
    bins <- cut(seq_along(mzs), breaks = n_bins, labels = FALSE)
    idx <- unlist(tapply(seq_along(mzs), bins, function(i) i[which.max(ints[i])]))
    idx <- sort(idx)
    mzs <- mzs[idx]
    ints <- ints[idx]
  }

  df <- data.frame(mz = mzs, intensity = ints)

  ## Plot
  p <- plotly::plot_ly(
    df, x = ~mz, y = ~intensity,
    type = "scattergl", mode = "lines",
    line = list(width = 1, color = "black"),
    hovertemplate = "m/z: %{x:.4f}<br>Intensity: %{y:.1f}<extra></extra>"
  ) |>
    plotly::layout(
      title = paste0(msa_name, " - spectrum ", pixel,
                     " (highest total ion count)"),
      xaxis = list(title = "<i>m/z</i>"),
      yaxis = list(title = "Intensity"),
      hovermode = "closest"
    )

  return(p)
}
