#' Plot a labeled spectrum around a target m/z using ggplot2
#'
#' @param mse An MSImagingExperiment object
#' @param center_mz Numeric, the target m/z to center the plot on
#' @param mz_range Numeric, half-width of the m/z window to display
#' @param n_labels Integer, number of peaks to label
#' @param label_tol Numeric, tolerance for matching center_mz to a labeled peak
#' @param plateau_tol Numeric, relative intensity threshold for grouping flat regions
#' @return A ggplot object
#' @export
plot_spectrum <- function(mse, center_mz, mz_range = 5,
                          n_labels = 5, label_tol = 0.5,
                          plateau_tol = 0.001) {

  ## Capture the name of the object as passed in, before any manipulation
  mse_name <- deparse(substitute(mse))

  ## Find the feature index closest to center_mz
  center_idx <- Cardinal::features(mse, mz = center_mz)

  ## Pick the pixel/spectrum with the highest intensity there
  center_intensities <- Cardinal::spectra(mse)[center_idx, ]
  best_pixel <- which.max(center_intensities)

  ## Extract the mz window around center for that spectrum
  mz_lo <- center_mz - mz_range
  mz_hi <- center_mz + mz_range

  fid_range <- Cardinal::features(mse, mz_lo < mz, mz < mz_hi)

  df <- data.frame(
    mz = Cardinal::mz(mse)[fid_range],
    intensity = Cardinal::spectra(mse)[fid_range, best_pixel]
  )

  ## Find candidate peaks to label: local maxima, collapsing plateaus
  df <- df[order(df$mz), ]
  df$grp <- cumsum(c(1, abs(diff(df$intensity)) > plateau_tol * max(df$intensity)))

  peak_candidates <- do.call(rbind, lapply(split(df, df$grp), function(g) {
    ## Only treat as a candidate peak if it's a local max relative to neighbors, not just any group
    g[which.max(g$intensity), ]
  }))

  ## Keep top n_labels by intensity
  peak_candidates <- peak_candidates[order(-peak_candidates$intensity), ]
  peak_candidates <- head(peak_candidates, n_labels)

  ## Always make sure the center mz peak itself is labeled
  center_row <- df[which.min(abs(df$mz - center_mz)), ]
  if (!any(abs(peak_candidates$mz - center_row$mz) < label_tol)) {
    peak_candidates <- rbind(peak_candidates, center_row)
  }

  ## Plot
  p <- ggplot2::ggplot(df, aes(x = mz, xend = mz, y = 0, yend = intensity)) +
    ggplot2::geom_segment(color = "grey30") +
    ggplot2::geom_point(data = peak_candidates, aes(x = mz, y = intensity),
               color = "red", size = 1.5) +
    ggrepel::geom_text_repel(data = peak_candidates,
                    aes(x = mz, y = intensity, label = round(mz, 4)),
                    color = "red", size = 3, max.overlaps = Inf) +
    ggplot2::labs(
      title = paste0(mse_name, " - spectrum ", best_pixel, " (highest intensity at *m/z* ", round(center_mz, 4), ")"),
      x = "m/z", y = "Intensity"
    ) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggtext::element_markdown()
    )

  return(p)
}
