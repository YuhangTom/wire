#' Summary data frame for the inner polygon
#'
#' Create the summary data frame for the inner polygon of an x3p object.
#' @param x3p x3p object
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in \code{concaveman::concaveman}
#' @param b positive integer value, block size, used in \code{x3ptools::x3p_average}
#' @param ifplot whether graphs are displayed
#' @return data frame of inside polygon
#' \itemize{
#'  \item x: x value from input x3p object
#'  \item y: y value from input x3p object
#'  \item value: height value from input x3p object
#'  \item mask: mask value from input x3p object
#'  \item n_neighbor_val_miss: number of missing immediate neighbor, self included
#'  \item sd_not_miss: standard deviation for immediate neighbor
#' }
#' @import dplyr
#' @importFrom x3ptools x3p_extract x3p_average x3p_to_df
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom raster raster adjacent ncell
#' @importFrom ggplot2 ggplot geom_raster scale_fill_gradient2 labs ggtitle geom_boxplot
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#'
#' x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = TRUE) %>%
#' str()
#'
x3p_insidepoly_df <- function(x3p, mask_col = "#FF0000", concavity = 1.5, b = 10,
                              ifplot = FALSE) {
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
  x3p_inner <- x3p_extract(x3p, mask_vals = mask_col) %>%
    x3p_average(b = b, na.rm = TRUE)

  x3p_inner_df <- x3p_inner %>%
    x3p_to_df()

  x3p_inner_raster <- t(x3p_inner$surface.matrix) %>%
    raster(xmx = (x3p_inner$header.info$sizeX - 1) * x3p_inner$header.info$incrementX, ymx = (x3p_inner$header.info$sizeY - 1) * x3p_inner$header.info$incrementY)

  ### Compute the adjacent (neighbor) cells with queen's case directions, including self
  x3p_inner_raster_adj <- adjacent(x3p_inner_raster, cells = 1:ncell(x3p_inner_raster), directions = 8, pairs = TRUE, sorted = TRUE, include = TRUE)

  x3p_inner_raster_adj_df <- x3p_inner_raster_adj %>%
    as_tibble() %>%
    mutate(
      neighbor_val = x3p_inner_raster[to]
    ) %>%
    group_by(
      from
    ) %>%
    summarise(
      ### Compute the number of missing cells in the neighbor
      n_neighbor_val_miss = neighbor_val %>%
        is.na() %>%
        sum(),
      ### Compute the sd of non-missing cells in the neighbor
      sd_not_miss = sd(neighbor_val, na.rm = TRUE)
    )


  ### Change it to wide format
  x3p_inner_df_wide_n_neighbor_val_miss <- matrix(x3p_inner_raster_adj_df$n_neighbor_val_miss, nrow = nrow(x3p_inner_raster), ncol = ncol(x3p_inner_raster), byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal")
  names(x3p_inner_df_wide_n_neighbor_val_miss) <- x3p_inner_df$x %>%
    unique() %>%
    as.character()

  ### Change it to wide format
  x3p_inner_df_wide_sd_not_miss <- matrix(x3p_inner_raster_adj_df$sd_not_miss, nrow = nrow(x3p_inner_raster), ncol = ncol(x3p_inner_raster), byrow = TRUE) %>%
    as_tibble(.name_repair = "minimal")
  names(x3p_inner_df_wide_sd_not_miss) <- x3p_inner_df$x %>%
    unique() %>%
    as.character()

  ### Change them to long format and append back
  x3p_inner_df <-
    full_join(
      x3p_inner_df %>%
        mutate(
          x = as.character(x),
          y = as.character(y)
        ),
      x3p_inner_df_wide_n_neighbor_val_miss %>%
        pivot_longer(
          cols = everything(),
          names_to = "x",
          values_to = "n_neighbor_val_miss"
        ) %>%
        mutate(
          y = x3p_inner_df$y,
          x = as.character(x),
          y = as.character(y)
        ),
      by = join_by(x, y)
    ) %>%
    full_join(
      x = .,
      x3p_inner_df_wide_sd_not_miss %>%
        pivot_longer(
          cols = everything(),
          names_to = "x",
          values_to = "sd_not_miss"
        ) %>%
        mutate(
          y = x3p_inner_df$y,
          x = as.character(x),
          y = as.character(y)
        ),
      by = join_by(x, y)
    ) %>%
    mutate(
      x = as.numeric(x),
      y = as.numeric(y),
      ### Discrete legend
      n_neighbor_val_miss = as.factor(n_neighbor_val_miss)
    )

  if (ifplot) {
    (x3p_inner_df %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradient2(midpoint = 4e-7)) %>%
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
