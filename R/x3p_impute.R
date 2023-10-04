#' Impute the inner polygon
#'
#' Obtained `x3p` object after imputing the inner polygon.
#' @param x3p `x3p` object
#' @param ifsave whether the imputation procedure gif is going to be saved
#' @param dir_name required when `ifsave` is `TRUE`
#' @param ifplot whether graphs are displayed
#' @return `x3p` object after imputation
#' @import dplyr
#' @importFrom x3ptools x3p_delete_mask x3p_extract x3p_average x3p_add_mask
#' @importFrom ggplot2 ggplot aes geom_raster scale_fill_gradient2 labs ggsave
#' @importFrom raster raster focal as.data.frame as.matrix
#' @importFrom purrr map
#' @importFrom magick image_read image_join image_animate image_write
#' @importFrom stringr str_detect
#' @importFrom assertthat assert_that is.flag not_empty is.string
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#'
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = TRUE)
#' x3p_inner_impute
#' if (interactive()) {
#'   x3p_image_autosize(x3p_inner_impute)
#' }
#'
x3p_impute <- function(x3p, ifsave = FALSE, dir_name = NULL, ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    is.flag(ifsave),
    is.flag(ifplot)
  )
  if (ifsave) {
    if (not_empty(dir_name)) {
      assert_that(
        is.string(dir_name)
      )
    } else {
    #  dir_name <- tempdir(check = TRUE)
      tmpfile <- tempfile(fileext = "txt")
      dir_name <- dirname(tmpfile)
    }

    dir.create(dir_name, showWarnings = FALSE)
  }

  layer <-
    x <-
    y <-
    value <-
    . <-
    NULL

  ### Convert x3p to raster
  x3p_inner_nomiss_res_raster <- t(x3p$surface.matrix) %>%
    raster(xmx = (x3p$header.info$sizeX - 1) * x3p$header.info$incrementX, ymx = (x3p$header.info$sizeY - 1) * x3p$header.info$incrementY)

  if (ifplot) {
    ### Plot raster
    p0 <- x3p_inner_nomiss_res_raster %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      rename(value = layer) %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradient2(midpoint = 4e-7) +
      labs(title = "Number of imputation: 0")

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
  # x3p_inner_nomiss_res_focal_raster %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_raster()


  if (ifplot) {
    ### First focal raster plot
    p1 <- x3p_inner_nomiss_res_focal_raster %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      rename(value = layer) %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_raster() +
      scale_fill_gradient2(midpoint = 4e-7) +
      labs(title = "Number of imputation: 1")

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
      .[str_detect(., pattern = "gif_p[:digit:]+.png")] %>%
      file.remove() %>%
      invisible()
  }

  x3p_inner_focal_impute <- x3p %>%
    x3p_delete_mask()
  x3p_inner_focal_impute$surface.matrix <- x3p_inner_nomiss_res_focal_raster %>%
    as.matrix() %>%
    t()

  x3p_inner_focal_impute <- x3p_add_mask(x3p_inner_focal_impute, x3p$mask)
  mask_col <- table(x3p$mask) %>%
    .[names(.) != "#FFFFFF"] %>%
    which.max() %>%
    names()

  x3p_inner_impute <- x3p_inner_focal_impute %>%
    x3p_extract(mask_vals = mask_col)

  return(x3p_inner_impute)
}
