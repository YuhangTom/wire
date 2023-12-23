#' Display an `x3p` object as an auto-sized image
#'
#' This function displays an `x3p` object as an image with automatically adjusted size based on the dimensions of the `x3p` object.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ifhtml A Boolean flag indicating whether the image will be embedded in HTML.
#' @param zoom A numeric value indicating the zoom level for the image display.
#' @param ... Additional parameters for `x3ptools::x3p_image`, excluding `size` and `zoom`.
#' @return An `rgl` plot. If `ifhtml = TRUE`, a list of HTML tags is returned.
#' @importFrom x3ptools x3p_image
#' @importFrom rgl rglwidget
#' @importFrom htmltools tagList
#' @importFrom assertthat assert_that is.flag is.number
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' if (interactive()) {
#'   x3p_image_autosize(x3p)
#' }
#'
x3p_image_autosize <- function(x3p, ifhtml = FALSE, zoom = 0.6, ...) {
  assert_that(
    "x3p" %in% class(x3p),
    is.flag(ifhtml),
    is.number(zoom), zoom > 0
  )

  x3p_image(x3p, size = dim(x3p$surface.matrix), zoom = zoom, ...)

  if (ifhtml) {
    rglwidget() %>%
      tagList() %>%
      return()
  }
}
