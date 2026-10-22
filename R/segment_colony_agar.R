#' This helper function uses Cardinal spatialShrunkenCentroids to automatically segment colony pixels from agar pixels.
#'
#' @param mse An MSImagingExperiment object
#' @param region_col  Integer, Maximum number of missing pixels to consider when determining pixel adjacency. If your regions are separated by at least 3 empty pixel-widths, set gap = 1 (strict, only truly adjacent pixels connect) so the gap correctly breaks them apart. Only increase gap if some single region's own acquisition has small internal skips you want bridged without splitting it.
#' @param r Integer, 1 Radius for spatialShrunkenCentroids
#' @param k Integer, 2 Max number of clusters for spatialShrunkenCentroids
#' @param s Integer, 0 Sparsity parameter for spatialShrunkenCentroids
#' @param auto_label Logical, TRUE, Whether the function will automatically label the pixels "colony" or "agar"
#' @param agar_class Character, NULL, USe this to provide the class representing the agar after SSC if known.
#' @return A factor containing the detected regions.
#' @export
segment_colony_agar <- function(mse, region_col = "region", r = 1, k = 2, s = 0,
                                auto_label = TRUE, agar_class = NULL) {

  if (is.null(mse[[region_col]])) {
    stop("Column '", region_col, "' not found in pixelData(mse). Run identify_regions() first.")
  }

  regions <- levels(factor(mse[[region_col]]))
  mse$colony_agar <- NA_character_

  for (reg in regions) {

    idx <- which(mse[[region_col]] == reg)
    if (length(idx) == 0) next

    mse_roi <- mse[, idx]

    set.seed(1)
    ssc <- spatialShrunkenCentroids(mse_roi, r = r, k = k, s = s)
    classes <- ssc[[1]]$class

    if (auto_label) {
      # use total ion current to decide which class is colony (higher signal) vs agar (lower)
      tic <- colSums(spectra(mse_roi), na.rm = TRUE)
      tic_by_class <- tapply(tic, classes, mean, na.rm = TRUE)
      agar_lvl <- names(which.min(tic_by_class))
    } else {
      if (is.null(agar_class)) {
        stop("auto_label = FALSE requires agar_class to be specified for region '", reg, "'")
      }
      agar_lvl <- as.character(agar_class)
    }

    label <- ifelse(as.character(classes) == agar_lvl, "agar", "colony")
    mse$colony_agar[idx] <- label
  }

  mse$colony_agar <- factor(mse$colony_agar)
  mse
}
