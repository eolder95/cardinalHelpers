#' This helper function defines regions given an mse object containing multiple independently collected regions of interest with disconnected coordinates.
#'
#' @param mse An MSImagingExperiment object
#' @param gap  Integer, Maximum number of missing pixels to consider when determining pixel adjacency. If your regions are separated by at least 3 empty pixel-widths, set gap = 1 (strict, only truly adjacent pixels connect) so the gap correctly breaks them apart. Only increase gap if some single region's own acquisition has small internal skips you want bridged without splitting it.
#' @param connectivity Integer, 8 (diagonal-inclusive) vs 4. If two regions' corners nearly touch diagonally, 8 could incorrectly merge them; 4 is stricter and safer if your regions are laid out on a grid with clean rectangular gaps between them.
#' @param min_size Integer, Minimum number of pixels to consider as a region. Set higher than 1 only if you expect stray single-pixel artifacts that shouldn't count as their own region.
#' @return A factor containing the detected regions.
#' @export
identify_regions <- function(mse, gap = 1, connectivity = 8, min_size = 1) {

  x <- coord(mse)$x
  y <- coord(mse)$y
  run_id <- if (!is.null(Cardinal::run(mse))) as.character(Cardinal::run(mse)) else rep("run1", length(x))

  n <- length(x)
  key <- paste(run_id, x, y, sep = "_")
  lookup <- setNames(seq_len(n), key)

  offsets <- if (connectivity == 4) {
    list(c(1, 0), c(0, 1))
  } else {
    list(c(1, 0), c(0, 1), c(1, 1), c(1, -1))
  }

  # --- build adjacency edges among all pixels, allowing gap-sized jumps ---
  edges <- integer(0)
  for (g in seq_len(gap)) {
    for (off in offsets) {
      nk <- paste(run_id, x + off[1] * g, y + off[2] * g, sep = "_")
      j <- lookup[nk]
      valid <- !is.na(j)
      if (any(valid)) {
        edges <- c(edges, rbind(which(valid), j[valid]))
      }
    }
  }

  # --- connected components ---
  gph <- igraph::make_graph(edges, n = n, directed = FALSE)
  comp <- igraph::components(gph)

  # --- drop tiny spurious regions if requested ---
  valid_regions <- which(comp$csize >= min_size)
  membership_filtered <- ifelse(comp$membership %in% valid_regions, comp$membership, NA)

  region_label <- ifelse(
    !is.na(membership_filtered),
    paste0("region", match(membership_filtered, valid_regions)),
    NA
  )

  factor(region_label)
}




### NOTE #### Add factor re-naming using forcats, given a vector containing region names in order
