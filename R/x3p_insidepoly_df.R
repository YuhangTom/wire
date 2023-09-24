#' Summary data frame for the inner polygon
#'
#' Create the summary data frame for the inner polygon of an x3p object.
#' @param x3p x3p object
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in \code{concaveman::concaveman}
#' @return data frame of inside polygon
#' @import dplyr
#' @importFrom x3ptools x3p_extract x3p_average x3p_to_df
#' @importFrom tidyr pivot_longer
#' @importFrom stats sd
#' @importFrom raster raster adjacent ncell
#' @importFrom wires x3p_surface_polygon
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' mask_col <- "#FF0000"
#' concavity <- 1.5
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = mask_col, concavity = concavity)
#' str(insidepoly_df)
#'
x3p_insidepoly_df <- function(x3p, mask_col = "#FF0000", concavity = 1.5) {
  to <-
    from <-
    neighbor_val <-
    x <-
    y <-
    n_neighbor_val_miss <-
    . <-
    NULL

  x3p <- x3p %>%
    x3p_surface_polygon(colour = mask_col, concavity = concavity)

  ### Extract inner part as x3p based on mask
  x3p_inner <- x3p_extract(x3p, mask_vals = mask_col) %>%
    x3p_average(b = 1, na.rm = TRUE)

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
    as_tibble()
  names(x3p_inner_df_wide_n_neighbor_val_miss) <- x3p_inner_df$x %>%
    unique() %>%
    as.character()

  ### Change it to wide format
  x3p_inner_df_wide_sd_not_miss <- matrix(x3p_inner_raster_adj_df$sd_not_miss, nrow = nrow(x3p_inner_raster), ncol = ncol(x3p_inner_raster), byrow = TRUE) %>%
    as_tibble()
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

  return(x3p_inner_df)
}