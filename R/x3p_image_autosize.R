#' Plot x3p object as an image with auto-adjusted size
#'
#' Plot x3p object as an image with auto-adjusted size based on \code{x3ptools::x3p_image}.
#' @param x3p x3p object
#' @param ifhtml logical, whether the image will be put on html
#' @param zoom numeric value indicating the amount of zoom
#' @param ... other parameter values except size and zoom used in \code{x3ptools::x3p_image}
#' @importFrom x3ptools x3p_image
#' @importFrom rgl rglwidget
#' @importFrom htmltools tagList
#' @export

x3p_image_autosize <- function(x3p, ifhtml = FALSE, zoom = 0.6, ...) {
  x3p_image(x3p, size = dim(x3p$surface.matrix), zoom = zoom, ...)

  if (ifhtml) {
    rglwidget() %>%
      tagList() %>%
      return()
  }
}
