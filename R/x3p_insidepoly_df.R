#' Generate summary data frame for inner polygon
#'
#' This function generates a summary data frame for the inner polygon of an `x3p` object.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param mask_col A string representing the color to be used for the polygon.
#' @param concavity A strictly positive number used in `concaveman::concaveman` to influence the shape of the polygon.
#' @param b A positive integer representing the block size for `x3ptools::x3p_average`.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes.
#' @return A data frame summarizing the inner polygon. The data frame includes the following columns:
#' * x: The `x` coordinates from the `x3p` object.
#' * y: The `y` coordinates from the `x3p` object.
#' * value: The height values from the `x3p` object.
#' * mask: The mask values from the `x3p` object.
#' * n_neighbor_val_miss: The number of immediate neighbors, including the point itself, that are missing.
#' * sd_not_miss: The standard deviation of the immediate neighbors that are not missing.
#' @import dplyr
#' @import ggplot2
#' @importFrom x3ptools x3p_extract x3p_average x3p_to_df x3p_get_scale
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom raster raster adjacent ncell focal
#' @importFrom assertthat assert_that is.string is.number is.count is.flag
#' @importFrom readr parse_number
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = TRUE)
#'
#' attr(insidepoly_df, "x3p_plot")
#' attr(insidepoly_df, "number_of_missing_immediate_neighbors_plot")
#' attr(insidepoly_df, "standard_deviation_of_non_missing_immediate_neighbors_plot")
#' attr(insidepoly_df, "number_of_missing_immediate_neighbors_boxplot")
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
    n_discrete <-
    NULL

  x3p <- x3p %>%
    x3p_surface_polygon(colour = mask_col, concavity = concavity)

  ### Extract inner part as x3p based on mask
  x3p_inner <- x3p_extract(x3p, mask_vals = mask_col)
  if (b > 1) {
    # only need to average when the block size is greater than 1
    x3p_inner <- x3p_inner %>% x3p_average(b = b, na.rm = TRUE)
  }

  resolution <- x3p_inner %>%
    x3p_get_scale()

  x3p_inner_df <- x3p_inner %>%
    x3p_to_df() %>%
    mutate(
      x = round(x / resolution),
      y = round(y / resolution)
    )

  x3p_inner_raster <- t(x3p_inner$surface.matrix) %>%
    raster(
      xmx = x3p_inner$header.info$sizeX - 1,
      ymx = x3p_inner$header.info$sizeY - 1
    )

  # x3p_inner_raster %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_tile()

  # counts the number of non-missing values:
  n_neighbor_val_miss <- focal(is.na(x3p_inner_raster), w = matrix(1, ncol = 3, nrow = 3), fun = sum)
  # n_neighbor_val_miss %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=factor(layer))) + geom_tile()
  # neighbors <- 9-n_neighbor_val_miss
  #  n_neighbor_val_mean <- focal(x3p_inner_raster, w=matrix(1, ncol=3, nrow=3), fun=sum, na.rm=TRUE)
  #  n_neighbor_val_mean <- n_neighbor_val_mean/neighbors
  #  n_neighbor_val_var <- focal((x3p_inner_raster-n_neighbor_val_mean)^2, w=matrix(1, ncol=3, nrow=3), fun=sum, na.rm=TRUE)
  # #neighbors[] <- pmax(0,neighbors[]-1)
  #  n_neighbor_val_sd <- sqrt(n_neighbor_val_var/9)
  #  n_neighbor_val_sd %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_tile()

  # # lines above are faster - but we need to adjust the denominator :(
  if (ifplot) {
    n_neighbor_val_sd <- focal(x3p_inner_raster, w = matrix(1, ncol = 3, nrow = 3), fun = sd, na.rm = TRUE)
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
    attr(x3p_inner_df, "x3p_plot") <- x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      theme_bw()

    attr(x3p_inner_df, "number_of_missing_immediate_neighbors_plot") <- x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = n_neighbor_val_miss)) +
      geom_tile() +
      labs(fill = "number") +
      ggtitle("Number of missing immediate neighbors (including self)") +
      theme_bw()

    attr(x3p_inner_df, "standard_deviation_of_non_missing_immediate_neighbors_plot") <- x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = sd_not_miss)) +
      geom_tile() +
      labs(fill = "sd") +
      ggtitle("Standard deviation of non-missing immediate neighbors (including self)") +
      theme_bw()

    attr(x3p_inner_df, "number_of_missing_immediate_neighbors_boxplot") <- x3p_inner_df %>%
      mutate(
        n_discrete = ifelse(parse_number(as.character(n_neighbor_val_miss)) <= 4, as.character(n_neighbor_val_miss), "5 or more")
      ) %>%
      filter(!is.na(n_discrete)) %>%
      ggplot(aes(x = n_discrete, y = sd_not_miss)) +
      geom_boxplot(fill = "grey80") +
      labs(
        x = "Number of missing immediate neighbors (including self)"
      ) +
      theme_bw() +
      scale_y_continuous("standard deviation", limits = c(0, .5))
  }

  return(x3p_inner_df)
}
