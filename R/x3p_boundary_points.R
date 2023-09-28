#' Identify boundary of a 3d topographic scan in `x3p` format
#'
#' @param x3p topographic scan in `x3p `format
#' @param sample positive integer value specifying the sampling ratio:
#' every `sample` value in `x` and `y` direction will be included.
#' Higher values result in faster results but cruder assessments of the boundary.
#' @return data frame of boundary points, variables are named `x` and `y`
#' @importFrom x3ptools x3p_sample x3p_to_df
#' @importFrom dplyr `%>%` group_by mutate filter summarize select
#' @importFrom assertthat assert_that is.count
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' bounds <- x3p_boundary_points(x3p, 2)
#'
#' library(ggplot2)
#' library(dplyr)
#' bounds %>%
#'   ggplot(aes(x = x, y = y)) +
#'   geom_point()
#'
x3p_boundary_points <- function(x3p, sample) {
  assert_that(
    "x3p" %in% class(x3p),
    is.count(sample)
  )

  y <-
    value <-
    x <-
    minx <-
    maxx <-
    miny <-
    maxy <-
    whatever <-
    NULL

  x3p_df <- x3p %>%
    x3p_sample(m = sample) %>%
    x3p_to_df()
  x_ranges <- x3p_df %>%
    group_by(y) %>%
    mutate(
      n = sum(!is.na(value))
    ) %>%
    filter(n > 0) %>%
    summarize(
      minx = min(x[!is.na(value)], na.rm = TRUE),
      maxx = max(x[!is.na(value)], na.rm = TRUE)
    )

  y_ranges <- x3p_df %>%
    group_by(x) %>%
    mutate(
      n = sum(!is.na(value))
    ) %>%
    filter(n > 0) %>%
    summarize(
      miny = min(y[!is.na(value)], na.rm = TRUE),
      maxy = max(y[!is.na(value)], na.rm = TRUE)
    )

  points <- rbind(
    x_ranges %>% pivot_longer(minx:maxx, values_to = "x", names_to = "whatever"),
    y_ranges %>% pivot_longer(miny:maxy, values_to = "y", names_to = "whatever")
  ) %>%
    filter(!is.infinite(x), !is.infinite(y)) %>%
    select(-whatever)

  return(points)
}
