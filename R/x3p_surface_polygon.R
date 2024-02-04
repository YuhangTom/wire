#' Add a polygon mask of the scan shape
#'
#' This function adds a polygon mask to an `x3p` scan. The polygon represents the shape of the scan.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param colour A string specifying the colour of the polygon mask.
#' @param sample A positive integer indicating the downsampling rate used to calculate the boundary points of the polygon. Higher values result in faster computation but less accurate polygons.
#' @param center A numeric vector representing the center point of the scan. If `NULL`, the center point is derived from the boundary points.
#' @param concavity A positive number used in the `concaveman::concaveman` function to determine the concavity of the polygon.
#' @return An `x3p` object with an added polygon mask.
#' @import x3ptools
#' @importFrom Cairo CairoPNG
#' @importFrom png readPNG
#' @importFrom grDevices as.raster dev.off
#' @importFrom graphics par plot.default polygon
#' @importFrom assertthat assert_that is.string is.count is.number
#' @export
#' @examples
#' if (interactive()) {
#'   x3p <- x3p_subsamples[[1]]
#'   x3p <- x3p %>% x3p_surface_polygon(sample=1)
#'   x3p_image(x3p, size = dim(x3p$surface.matrix), zoom=.6)
#' }
#'
x3p_surface_polygon <- function(x3p, colour = "red", sample = 10, center = NULL, concavity = 1.5) {
  assert_that(
    "x3p" %in% class(x3p),
    is.string(colour),
    is.count(sample),
    is.number(concavity), concavity > 0
  )

  boundary <- x3p %>% x3p_boundary_points(sample = sample)
  polygon_inside <- inside_polygon(boundary$x, boundary$y,
    concavity = concavity,
    center = center
  )
  dims <- dim(x3p$surface.matrix)
  resolution <- x3p %>% x3p_get_scale()

  mask_png <- tempfile(fileext = ".png")

  CairoPNG(mask_png, width = dims[1], height = dims[2], units = "px")
  par(mar = c(0, 0, 0, 0))
  plot.default(x = c(1, dims[1]) * resolution, y = c(1, dims[2]) * resolution, type = "n", frame.plot = FALSE, axes = FALSE)
  polygon(rbind(polygon_inside[, c("x", "y")], polygon_inside[1, c("x", "y")]), col = colour, border = FALSE)
  dev.off()
  mask <- png::readPNG(mask_png)

  # this replaces any existing mask

  x3p_add_mask(x3p, mask = as.raster(mask)) %>%
    x3p_trim_na(ratio = 1)
}
