#' Impute missing values
#'
#' This function imputes missing values in an `x3p` object.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ifout A Boolean flag indicating whether the imputation procedure should extrapolate. Set to `TRUE` for extrapolation.
#' @param ifsave A Boolean flag indicating whether to save the imputation procedure gif.
#' @param dir_name A string representing the directory name where the gif should be saved. Required when `ifsave = TRUE`.
#' @param gif_name A string representing the gif name. Required when `ifsave = TRUE`.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes. Automatically set to `TRUE` when `ifsave = TRUE`.
#' @return An `x3p` object after imputation.
#' @import dplyr
#' @import ggplot2
#' @import x3ptools
#' @importFrom raster raster focal as.data.frame as.matrix
#' @importFrom purrr map
#' @importFrom magick image_read image_join image_animate image_write
#' @importFrom stringr str_detect
#' @importFrom assertthat assert_that is.flag not_empty is.string
#' @importFrom stringr str_remove
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#'
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = TRUE
#' )
#'
#' attr(x3p_inner_impute, "x3p_impute_0_plot")
#' attr(x3p_inner_impute, "x3p_impute_1_plot")
#' attr(x3p_inner_impute, "x3p_impute_n_plot")
#'
#' if (interactive()) {
#'   x3p_image_autosize(x3p_inner_impute)
#' }
#'
x3p_impute <- function(x3p, ifout = FALSE, ifsave = FALSE, dir_name = NULL, gif_name = NULL, ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    is.flag(ifout),
    is.flag(ifsave),
    is.flag(ifplot)
  )
  if (ifsave) {
    ifplot <- TRUE

    if (not_empty(gif_name)) {
      assert_that(
        is.string(gif_name)
      )
    } else {
      gif_name <- "focal_impute.gif"
    }

    if (not_empty(dir_name)) {
      assert_that(
        is.string(dir_name)
      )
      dir_name <- str_remove(dir_name, "/$")
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
    x_plot <-
    y_plot <-
    value <-
    . <-
    NULL

  ggplot_attrs <- NA

  ### Convert x3p to raster
  x3p_inner_nomiss_res_raster <- x3p$surface.matrix %>%
    raster(
      ymx = x3p$header.info$sizeX - 1,
      xmx = x3p$header.info$sizeY - 1
    )

  resolution <- x3p %>%
    x3p_get_scale()

  if (ifplot) {
    attr(ggplot_attrs, "x3p_impute_0_plot") <- x3p_inner_nomiss_res_raster %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      mutate(
        x_plot = (-y + max(y)) * resolution,
        y_plot = (-x + max(x)) * resolution
      ) %>%
      rename(value = layer) %>%
      ggplot(aes(x = x_plot, y = y_plot, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      labs(
        title = "Number of imputation: 0",
        fill = expression(paste("value (", mu, "m)"))
      ) +
      xlab(expression(paste("x (", mu, "m)"))) +
      ylab(expression(paste("y (", mu, "m)"))) +
      theme_bw()
  }

  if (ifsave) {
    ggsave(paste0(dir_name, "/gif_p0.png"), attr(ggplot_attrs, "x3p_impute_0_plot"), width = 5, height = 3)
  }

  ### Get number of missing values
  nNA <- table(is.na(x3p_inner_nomiss_res_raster[]))["TRUE"]

  ### Initialize focal raster
  #  browser()
  #  p1 <- proc.time()
  ns <- focal(!is.na(x3p_inner_nomiss_res_raster),
    fun = sum, w = matrix(1, nrow = 3, ncol = 3)
  )
  sums <- focal(x3p_inner_nomiss_res_raster,
    fun = sum, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE
  )
  ns[!(is.na(x3p_inner_nomiss_res_raster[]))] <- 1
  x3p_inner_nomiss_res_focal_raster <- sums / ns
  #  p2 <- proc.time()

  # p3 <- proc.time()
  # x3p_inner_nomiss_res_focal_raster <-
  #   focal(x3p_inner_nomiss_res_raster,
  #     fun = function(x, na.rm) {
  #       mean(x, na.rm = TRUE)
  #     }, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE)
  # p4 <- proc.time()

  # x3p_inner_nomiss_res_focal_raster %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_tile()
  #  x3p_inner_nomiss_res_focal_raster %>% as.data.frame(xy=TRUE) %>% ggplot(aes(x=x, y=y, fill=layer)) + geom_tile()



  if (ifplot) {
    attr(ggplot_attrs, "x3p_impute_1_plot") <- x3p_inner_nomiss_res_focal_raster %>%
      as.data.frame(xy = TRUE) %>%
      as_tibble() %>%
      mutate(
        x_plot = (-y + max(y)) * resolution,
        y_plot = (-x + max(x)) * resolution
      ) %>%
      rename(value = layer) %>%
      ggplot(aes(x = x_plot, y = y_plot, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      labs(
        title = "Number of imputation: 1",
        fill = expression(paste("value (", mu, "m)"))
      ) +
      xlab(expression(paste("x (", mu, "m)"))) +
      ylab(expression(paste("y (", mu, "m)"))) +
      theme_bw()
  }

  if (ifsave) {
    ggsave(paste0(dir_name, "/gif_p1.png"), attr(ggplot_attrs, "x3p_impute_1_plot"), width = 5, height = 3)
  }

  ### Initialize condition
  do <- TRUE

  nimp <- 1

  ### For saving result
  x3p_inner_focal_impute <- x3p %>%
    x3p_delete_mask()
  mask_col <- table(x3p$mask) %>%
    .[names(.) != "#FFFFFF"] %>%
    which.max() %>%
    names()
  in_mask <- t(x3p$mask == mask_col)

  while (do) {
    ### Get number of missing value
    nNA <- table(is.na(x3p_inner_nomiss_res_focal_raster[]))["TRUE"]

    ### Focal raster
    #    x3p_inner_nomiss_res_focal_raster <- focal(x3p_inner_nomiss_res_focal_raster, fun = function(x, na.rm) {
    #      mean(x, na.rm = TRUE)
    #   }, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE)
    ns <- focal(!is.na(x3p_inner_nomiss_res_focal_raster),
      fun = sum, w = matrix(1, nrow = 3, ncol = 3)
    )
    sums <- focal(x3p_inner_nomiss_res_focal_raster,
      fun = sum, w = matrix(1, nrow = 3, ncol = 3), na.rm = TRUE, NAonly = TRUE
    )
    ns[!(is.na(x3p_inner_nomiss_res_focal_raster[]))] <- 1
    x3p_inner_nomiss_res_focal_raster <- sums / ns




    nimp <- nimp + 1

    if (ifplot) {
      attr(ggplot_attrs, "x3p_impute_n_plot") <- x3p_inner_nomiss_res_focal_raster %>%
        as.data.frame(xy = TRUE) %>%
        as_tibble() %>%
        mutate(
          x_plot = (-y + max(y)) * resolution,
          y_plot = (-x + max(x)) * resolution
        ) %>%
        rename(value = layer) %>%
        ggplot(aes(x = x_plot, y = y_plot, fill = value)) +
        geom_tile() +
        scale_fill_gradient2(midpoint = 0) +
        labs(
          title = paste0("Number of imputation: ", nimp),
          fill = expression(paste("value (", mu, "m)"))
        ) +
        xlab(expression(paste("x (", mu, "m)"))) +
        ylab(expression(paste("y (", mu, "m)"))) +
        theme_bw()
    }

    if (ifsave) {
      ggsave(paste0(dir_name, "/gif_p", nimp, ".png"), attr(ggplot_attrs, "x3p_impute_n_plot"), width = 5, height = 3)
    }

    ### Check condition
    if (ifout) {
      if (table(is.na(x3p_inner_nomiss_res_focal_raster[]))["TRUE"] %>%
        near(nNA)
      ) {
        do <- FALSE
      }
    } else {
      x3p_inner_focal_impute$surface.matrix <- x3p_inner_nomiss_res_focal_raster %>%
        as.matrix()

      if (x3p_inner_focal_impute$surface.matrix[in_mask] %>%
        is.na() %>%
        sum() %>%
        near(0)
      ) {
        do <- FALSE
      }
    }
  }

  if (ifsave) {
    ### List file names and read in
    x3p_inner_nomiss_res_focal_impute_gif <- paste0(dir_name, "/gif_p", c(rep(0, 4), 0:nimp, rep(nimp, 9)), ".png") %>%
      map(image_read) %>%
      ### Join the images together
      image_join() %>%
      ### Generate gif with default FPS
      image_animate()

    ### Save gif to disk
    image_write(
      image = x3p_inner_nomiss_res_focal_impute_gif,
      path = paste0(dir_name, "/", gif_name)
    )

    list.files(
      path = dir_name,
      full.names = TRUE
    ) %>%
      .[str_detect(., pattern = "gif_p[:digit:]+.png")] %>%
      file.remove() %>%
      invisible()
  }

  x3p_inner_focal_impute$surface.matrix <- x3p_inner_nomiss_res_focal_raster %>%
    as.matrix()

  x3p_inner_focal_impute <- x3p_add_mask(x3p_inner_focal_impute, x3p$mask)

  x3p_inner_impute <- x3p_inner_focal_impute %>%
    x3p_extract(mask_vals = mask_col) %>%
    x3p_delete_mask()

  attributes(x3p_inner_impute) <- c(attributes(x3p_inner_impute), attributes(ggplot_attrs))

  return(x3p_inner_impute)
}
