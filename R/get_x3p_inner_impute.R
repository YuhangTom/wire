#' Impute the inner polygon
#'
#' Obtained x3p object after imputing the inner polygon.
#' @param x3p x3p object
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in \code{concaveman::concaveman}
#' @param ifsave whether the imputation procedure gif is going to be saved
#' @param dir_name required when \code{ifsave} is \code{TRUE}
#' @param ifplot whether graphs are displayed
#' @import dplyr x3ptools
#' @importFrom assertthat assert_that not_empty
#' @importFrom raster raster focal as.data.frame
#' @importFrom purrr map
#' @importFrom magick image_join image_animate image_write
#' @importFrom stringr str_detect
#' @export

get_x3p_inner_impute <- function(x3p, mask_col = "#FF0000", concavity = 1.5,
                                 ifsave = FALSE, dir_name = NULL, ifplot = FALSE) {
  if (ifsave) {
    assert_that(not_empty(dir_name), msg = "dir_name must be non-empty")
    assert_that(is.character(dir_name), msg = "dir_name must be character")
    dir.create(dir_name, showWarnings = FALSE)
  }

  x3p_inner_nomiss_res <- get_x3p_inner_nomiss_res(x3p, mask_col = mask_col, concavity = concavity)

  ### Convert x3p to raster
  x3p_inner_nomiss_res_raster <- t(x3p_inner_nomiss_res$surface.matrix) %>%
    raster(xmx = (x3p_inner_nomiss_res$header.info$sizeX - 1) * x3p_inner_nomiss_res$header.info$incrementX, ymx = (x3p_inner_nomiss_res$header.info$sizeY - 1) * x3p_inner_nomiss_res$header.info$incrementY)

  ### Plot raster
  p0 <- x3p_inner_nomiss_res_raster %>%
    as.data.frame(xy = TRUE) %>%
    as_tibble() %>%
    rename(value = layer) %>%
    ggplot(aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradient2(midpoint = 4e-7) +
    labs(title = x3p_name, subtitle = "Number of imputation: 0")

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
    labs(title = x3p_name, subtitle = "Number of imputation: 1")

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
      labs(title = x3p_name, subtitle = paste0("Number of imputation: ", nimp))

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

  x3p_inner_focal_impute <- x3p_inner_nomiss_res %>%
    x3p_delete_mask()
  x3p_inner_focal_impute$surface.matrix <- x3p_inner_nomiss_res_focal_raster %>%
    as.matrix() %>%
    t()

  x3p_inner <- x3p_inner_df %>%
    df_to_x3p()
  x3p_inner_focal_impute <- x3p_add_mask(x3p_inner_focal_impute, x3p_inner$mask)

  x3p_inner_impute <- x3p_inner_focal_impute %>%
    x3p_extract(mask_vals = mask_col)

  return(x3p_inner_impute)
}
