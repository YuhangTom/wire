#' Summary data frame for the inner polygon
#'
#' Create the summary data frame for the inner polygon of an `x3p` object.
#' @param x3p `x3p` object
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in `concaveman::concaveman`
#' @param b positive integer value, block size, used in `x3ptools::x3p_average`
#' @param ifplot whether graphs are displayed
#' @return data frame of inside polygon
#' * x: `x` value from input `x3p` object
#' * y: `y` value from input `x3p` object
#' * value: height value from input `x3p` object
#' * mask: mask value from input `x3p` object
#' * n_neighbor_val_miss: number of missing immediate neighbor, self included
#' * sd_not_miss: standard deviation for immediate neighbor
#' @import dplyr
#' @importFrom x3ptools x3p_extract x3p_average x3p_to_df
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom raster raster adjacent ncell focal
#' @importFrom ggplot2 ggplot geom_raster scale_fill_gradient2 labs ggtitle geom_boxplot
#' @importFrom assertthat assert_that is.string is.number is.count is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#'
#' x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 10, ifplot = TRUE) %>%
#'   str()
#'
x3p_insidepoly_df <- function(x3p, mask_col = "#FF0000", concavity = 1.5, b = 10,
                              ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    is.string(mask_col),
    is.number(concavity), concavity > 0,
    is.count(b),
    is.flag(ifplot)
  )

  to <-
    from <-
    neighbor_val <-
    x <-
    y <-
    n_neighbor_val_miss <-
    value <-
    sd_not_miss <-
    . <-
    NULL

  x3p <- x3p %>%
    x3p_surface_polygon(colour = mask_col, concavity = concavity)

  ### Extract inner part as x3p based on mask
  x3p_inner <- x3p_extract(x3p, mask_vals = mask_col)
  if (b > 1) {
    # only need to average when the block size is greater than 1
    x3p_inner <- x3p_inner %>% x3p_average(b = b, na.rm = TRUE)
  }

  resolution <- x3p_inner %>% x3p_get_scale()

  x3p_inner_df <- x3p_inner %>%
    x3p_to_df() %>%
    mutate(
      x = round(x / resolution),
      y = round(y / resolution)
    )

  x3p_inner_raster <- t(x3p_inner$surface.matrix) %>%
    raster(xmx = (x3p_inner$header.info$sizeX - 1), ymx = (x3p_inner$header.info$sizeY - 1))

  # x3p_inner_raster %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_raster()

  # counts the number of non-missing values:
  n_neighbor_val_miss <- focal(is.na(x3p_inner_raster), w = matrix(1, ncol = 3, nrow = 3), fun = sum)
  # n_neighbor_val_miss %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=factor(layer))) + geom_raster()

  #  neighbors <- 9-n_neighbor_val_miss
  #  n_neighbor_val_mean <- focal(x3p_inner_raster, w=matrix(1, ncol=3, nrow=3), fun=sum, na.rm=TRUE)
  #  n_neighbor_val_mean <- n_neighbor_val_mean/neighbors
  #  n_neighbor_val_var <- focal((x3p_inner_raster-n_neighbor_val_mean)^2, w=matrix(1, ncol=3, nrow=3), fun=sum, na.rm=TRUE)
  # #neighbors[] <- pmax(0,neighbors[]-1)
  #  n_neighbor_val_sd <- sqrt(n_neighbor_val_var/9)
  #  n_neighbor_val_sd %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_raster()

  # # lines above are faster - but we need to adjust the denominator :(
  if (ifplot) {
    n_neighbor_val_sd <- focal(x3p_inner_raster, w = matrix(1, nc = 3, nr = 3), fun = sd, na.rm = TRUE)
  }

  layer <- NULL
  n_neighbor_df <- n_neighbor_val_miss %>%
    as.data.frame(xy = TRUE) %>%
    rename(n_neighbor_val_miss = layer) %>%
    mutate(x = round(x), y = round(y))

  if (ifplot) {
    n_neighbor_sd_df <- n_neighbor_val_sd %>%
      as.data.frame(xy = TRUE) %>%
      rename(sd_not_miss = layer) %>%
      mutate(x = round(x), y = round(y))
  }

  #  x3p_inner_raster_adj_df <- n_neighbor_df %>% left_join(n_neighbor_sd_df, by=c("x", "y"))

  # ### Compute the adjacent (neighbor) cells with queen's case directions, including self
  # x3p_inner_raster_adj <- adjacent(x3p_inner_raster, cells = 1:ncell(x3p_inner_raster), directions = 8, pairs = TRUE, sorted = TRUE, include = TRUE)
  #
  # x3p_inner_raster_adj_df <- x3p_inner_raster_adj %>%
  #   as_tibble() %>%
  #   mutate(
  #     neighbor_val = x3p_inner_raster[to]
  #   ) %>%
  #   group_by(
  #     from
  #   ) %>%
  #   summarise(
  #     ### Compute the number of missing cells in the neighbor
  #     n_neighbor_val_miss = neighbor_val %>%
  #       is.na() %>%
  #       sum(),
  #     ### Compute the sd of non-missing cells in the neighbor
  #     sd_not_miss = sd(neighbor_val, na.rm = TRUE)
  #   )

  if (ifplot) {
    x3p_inner_df <- x3p_inner_df %>%
      left_join(n_neighbor_df, by = c("x", "y")) %>%
      left_join(n_neighbor_sd_df, by = c("x", "y")) %>%
      mutate(
        n_neighbor_val_miss = factor(n_neighbor_val_miss),
        x = x * resolution,
        y = y * resolution
      )
  } else {
    x3p_inner_df <- x3p_inner_df %>%
      left_join(n_neighbor_df, by = c("x", "y")) %>%
      mutate(
        n_neighbor_val_miss = factor(n_neighbor_val_miss),
        x = x * resolution,
        y = y * resolution
      )
  }

  if (ifplot) {
    (x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradient2(midpoint = 0)) %>%
      print()

    ### ggplot
    (x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = n_neighbor_val_miss)) +
      geom_raster() +
      labs(fill = "number") +
      ggtitle("Number of missing immediate neighbors (including self)")) %>%
      print()

    ### ggplot
    (x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = sd_not_miss)) +
      geom_raster() +
      labs(fill = "sd") +
      ggtitle("Standard deviation of non-missing immediate neighbors (including self)")) %>%
      print()

    (x3p_inner_df %>%
      ggplot(aes(x = n_neighbor_val_miss, y = log(sd_not_miss))) +
      geom_boxplot() +
      labs(
        x = "Number of missing immediate neighbors (including self)",
        y = "log(standard deviation)"
      )) %>%
      print()
  }

  return(x3p_inner_df)
}
