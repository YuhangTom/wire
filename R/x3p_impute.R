#' Impute the inner polygon
#'
#' Obtained x3p object after imputing the inner polygon.
#' @param x3p x3p object
#' @param x3p_mask x3p object for mask
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in \code{concaveman::concaveman}
#' @param ifsave whether the imputation procedure gif is going to be saved
#' @param dir_name required when \code{ifsave} is \code{TRUE}
#' @param ifplot whether graphs are displayed
#' @return x3p object after imputation
#' @import dplyr
#' @importFrom x3ptools x3p_delete_mask x3p_extract x3p_average x3p_add_mask
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradient2 labs ggsave
#' @importFrom assertthat assert_that not_empty
#' @importFrom raster raster focal as.data.frame as.matrix
#' @importFrom purrr map
#' @importFrom magick image_read image_join image_animate image_write
#' @importFrom stringr str_detect
#' @importFrom wires x3p_surface_polygon
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' mask_col <- "#FF0000"
#' concavity <- 1.5
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = mask_col, concavity = concavity)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#'
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, x3p, mask_col = mask_col,
#' concavity = concavity, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
#' x3p_inner_impute
#'
#' if (interactive()) {
#'   x3p_image_autosize(x3p_inner_impute)
#' }
#'
x3p_impute <- function(x3p, x3p_mask, mask_col = "#FF0000",
                       concavity = 1.5, ifsave = FALSE, dir_name = NULL, ifplot = FALSE) {
  layer <-
    x <-
    y <-
    value <-
    . <-
    NULL

  if (ifsave) {
    assert_that(not_empty(dir_name), msg = "dir_name must be non-empty")
    assert_that(is.character(dir_name), msg = "dir_name must be character")
    dir.create(dir_name, showWarnings = FALSE)
  }

  ### Convert x3p to raster
  x3p_inner_nomiss_res_raster <- t(x3p$surface.matrix) %>%
    raster(xmx = (x3p$header.info$sizeX - 1) * x3p$header.info$incrementX, ymx = (x3p$header.info$sizeY - 1) * x3p$header.info$incrementY)

  ### Plot raster
  p0 <- x3p_inner_nomiss_res_raster %>%
    as.data.frame(xy = TRUE) %>%
    as_tibble() %>%
    rename(value = layer) %>%
    ggplot(aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 4e-7) +
    labs(title = "Number of imputation: 0")

  if (ifplot) {
    print(p0)
  }

  if (ifsave) {
    ggsave(paste0(dir_name, "/gif_p0.png"), p0, width = 5, height = 3)
  }

  ### Get number of missing values
  nNA <- table(is.na(x3p_inner_nomiss_res_raster[]))[2]

  ### Initialize focal raster
  x3p_inner_nomiss_res_focal_raster <- focal(x3p_inner_nomiss_res_raster, fun = function(x, na.rm) {
    mean(x, na.rm = TRUE)
  }, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE)

  ### First focal raster plot
  p1 <- x3p_inner_nomiss_res_focal_raster %>%
    as.data.frame(xy = TRUE) %>%
    as_tibble() %>%
    rename(value = layer) %>%
    ggplot(aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 4e-7) +
    labs(title = "Number of imputation: 1")

  if (ifplot) {
    print(p1)
  }

  if (ifsave) {
    ggsave(paste0(dir_name, "/gif_p1.png"), p1, width = 5, height = 3)
  }

  ### Initialize condition
  do <- TRUE

  nimp <- 1

  while (do) {
    ### Get number of missing value
    nNA <- table(is.na(x3p_inner_nomiss_res_focal_raster[]))[2]

    ### Focal raster
    x3p_inner_nomiss_res_focal_raster <- focal(x3p_inner_nomiss_res_focal_raster, fun = function(x, na.rm) {
      mean(x, na.rm = TRUE)
    }, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE)

    nimp <- nimp + 1

    p <- x3p_inner_nomiss_res_focal_raster %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      rename(value = layer) %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradient2(midpoint = 4e-7) +
      labs(title = paste0("Number of imputation: ", nimp))

    if (ifsave) {
      ggsave(paste0(dir_name, "/gif_p", nimp, ".png"), p, width = 5, height = 3)
    }

    ### Check condition
    do <- table(is.na(x3p_inner_nomiss_res_focal_raster[]))[2] != nNA
  }

  ### Plot final raster
  if (ifplot) {
    print(p)
  }

  if (ifsave) {
    ### List file names and read in
    x3p_inner_nomiss_res_focal_impute_gif <- paste0(dir_name, "/gif_p", 0:nimp, ".png") %>%
      map(image_read) %>%
      ### Join the images together
      image_join() %>%
      ### Generate gif with default FPS
      image_animate()

    ### Save gif to disk
    image_write(
      image = x3p_inner_nomiss_res_focal_impute_gif,
      path = paste0(dir_name, "/focal_impute.gif")
    )

    list.files(
      path = dir_name,
      full.names = TRUE
    ) %>%
      .[str_detect(., pattern = ".png")] %>%
      file.remove() %>%
      invisible()
  }

  x3p_inner_focal_impute <- x3p_mask %>%
    x3p_delete_mask()
  x3p_inner_focal_impute$surface.matrix <- x3p_inner_nomiss_res_focal_raster %>%
    as.matrix() %>%
    t()

  x3p_mask <- x3p_mask %>%
    x3p_surface_polygon(colour = mask_col, concavity = concavity)
  ### Extract inner part as x3p based on mask
  x3p_inner <- x3p_extract(x3p_mask, mask_vals = mask_col) %>%
    x3p_average(b = 1, na.rm = TRUE)
  x3p_inner_focal_impute <- x3p_add_mask(x3p_inner_focal_impute, x3p_inner$mask)

  x3p_inner_impute <- x3p_inner_focal_impute %>%
    x3p_extract(mask_vals = mask_col)

  return(x3p_inner_impute)
}
