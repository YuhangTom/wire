#' Plot `x3p` object as an image with auto-adjusted size
#'
#' Plot `x3p` object as an image with auto-adjusted size based on `x3ptools::x3p_image`.
#' @param x3p `x3p` object
#' @param ifhtml logical, whether the image will be put on html
#' @param zoom numeric value indicating the amount of `zoom` in `x3ptools::x3p_image`
#' @param ... other parameter values except `size` and `zoom` used in `x3ptools::x3p_image`
#' @return `rgl` plot, list of tags when `ifhtml = TRUE`
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
