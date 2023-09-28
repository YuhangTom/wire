#' Identify the inside polygon of a set of points
#'
#' Uses concentric mirroring onto a radius around the center point, applies an
#' alpha hull to that shape, and mirrors the result back.
#' @param x numeric vector of `x` positions
#' @param y numeric vector of `y` positions
#' @param concavity strictly positive parameter. Smaller values approach the
#' inside of the boundary
#' more closely. Large values concentrate on the inner center of the area
#' @param center numeric vector of length 2, consisting of `(x,y)` coordinate for
#' centering `x` and `y` values.
#' If center is `NULL`, the half ranges of `x` and `y` are used.
#' @return data frame of `x` and `y` positions describing the inside of the area
#' described by the input `x` and `y`, variable id describes the order of the points
#' @importFrom dplyr mutate select rename arrange n near
#' @importFrom concaveman concaveman
#' @importFrom tidyr pivot_longer
#' @importFrom assertthat assert_that is.number
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' bounds <- x3p_boundary_points(x3p, 2)
#' polygon <- inside_polygon(bounds$x, bounds$y, 1)
#'
#' library(ggplot2)
#' library(dplyr)
#' bounds %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point() +
#'   geom_polygon(data = polygon)
#'
inside_polygon <- function(x, y, concavity, center = NULL) {
  assert_that(
    is.numeric(x),
    is.numeric(y),
    is.number(concavity), concavity > 0
  )

  theta <-
    r <-
    xout <-
    yout <-
    V1 <-
    V2 <-
    rout <-
    NULL

  if (is.null(center)) {
    center <- c(diff(range(x, na.rm = TRUE)), diff(range(y, na.rm = TRUE))) / 2
  }
  if (length(center) == 1) center <- rep(center, 2)
  assert_that(
    is.numeric(center), near(length(center), 2)
  )

  points_inside_out <- data.frame(x, y) %>%
    mutate(x = x - center[1], y = y - center[2]) %>%
    mutate(
      r = sqrt(x^2 + y^2),
      theta = atan(y / x),
      theta = ifelse(x < 0, atan(y / x) + pi, ifelse(y < 0, theta + 2 * pi, theta))
    ) %>%
    mutate(
      xout = 1 / r * cos(theta),
      yout = 1 / r * sin(theta)
    )

  points_mat_left <- points_inside_out %>%
    select(xout, yout) %>%
    as.matrix()
  polygons_left <- concaveman(points_mat_left, concavity = concavity)

  points_mat_right <- points_inside_out %>%
    select(xout, yout) %>%
    mutate(xout = -xout) %>%
    as.matrix()
  polygons_right <- concaveman(points_mat_right, concavity = concavity)

  polygon_inside_out <- rbind(
    data.frame(polygons_left) %>% filter(V1 < 0),
    data.frame(polygons_right) %>% filter(V1 < 0) %>% mutate(V1 = -V1)
  )

  polygon_inside_out <- polygon_inside_out %>%
    rename(xout = V1, yout = V2)

  polygon_inside_out <- polygon_inside_out %>%
    mutate(
      rout = sqrt(xout^2 + yout^2),
      theta = atan(yout / xout),
      theta = ifelse(xout < 0, theta + pi,
        ifelse(yout < 0, theta + 2 * pi, theta)
      )
    ) %>%
    mutate(
      x = 1 / rout * cos(theta),
      y = 1 / rout * sin(theta)
    )
  polygon_inside_out <- polygon_inside_out %>%
    arrange(theta) %>%
    mutate(id = 1:n())
  polygon_inside_out %>% mutate(
    x = x + center[1],
    y = y + center[2]
  )
}
