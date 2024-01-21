#' Calculate rotation angle using maximum likelihood estimation (MLE)
#'
#' This function calculates the rotation angle of an `x3p` object using maximum likelihood estimation (MLE) with Hough transformation.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ntheta The number of bins along the theta axis used in `imager::hough_line`.
#' @param min_score_cut A tuning parameter that sets the minimum score required for the Hough transformation.
#' @param ifplot A Boolean flag indicating whether to display graphs.
#' @param loess_span A parameter controlling the degree of smoothing in the LOESS function.
#' @return A vector of rotation angles computed by the MLE procedure.
#' @import dplyr
#' @import ggplot2
#' @importFrom raster raster
#' @importFrom imager as.cimg hough_line
#' @importFrom stats loess predict
#' @importFrom assertthat assert_that is.count is.number is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#'
#' x3p_bin <- x3p_inner_impute %>%
#'   x3ptools::x3p_bin_stripes(
#'     direction = "vertical",
#'     colors = c("#b12819", "#ffffff", "#134D6B"),
#'     freqs = c(0, 0.3, 0.7, 1)
#'   )
#' x3p_bin_red <- x3ptools::x3p_extract(x3p_bin, mask_vals = "#b12819")
#'
#' x3p_MLE_angle_vec(x3p_bin_red, min_score_cut = 5, ifplot = TRUE) %>%
#'   str()
#'
x3p_MLE_angle_vec <- function(x3p, ntheta = 720, min_score_cut = 0.1,
                              ifplot = FALSE,
                              loess_span = 0.2) {
  assert_that(
    "x3p" %in% class(x3p),
    is.count(ntheta),
    is.number(min_score_cut),
    is.flag(ifplot),
    is.number(loess_span), loess_span > 0
  )

  theta <-
    score <-
    theta_mod <-
    theta_mod_shift <-
    rho <-
    x <-
    y <-
    value <-
    slope <-
    intercept <-
    . <-
    NULL

  ### Change to contrast color
  x3p_shift <- x3p$surface.matrix
  NA_val <- -(x3p$surface.matrix %>%
    c() %>%
    summary() %>%
    .[c("Min.", "Max.")] %>%
    abs() %>%
    max() %>%
    ceiling(.))
  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val

  ### Change to raster
  x3p_raster <- t(x3p_shift) %>%
    raster(xmx = x3p$header.info$sizeX - 1, ymx = x3p$header.info$sizeY - 1)

  ### Change to cimg
  x3p_cimg <- as.cimg(x3p_raster)

  ### Hough transformation for lines
  x3p_hough_df <- hough_line(x3p_cimg, ntheta = ntheta, data.frame = TRUE, shift = FALSE)

  ### For theta_mod: 0 = pi = 2 * pi <- pi / 2, pi / 2 = pi * 3 / 2 <- 0
  ### For theta_mod_shift: 0 = pi = 2 * pi <- pi, pi / 2 = pi * 3 / 2 <- pi / 2
  ### For theta_mod_shift / pi: 0 = pi = 2 * pi <- 1, pi / 2 = pi * 3 / 2 <- 1 / 2
  x3p_hough_df_shift <- x3p_hough_df %>%
    mutate(
      theta_mod = (theta - pi / 2) %% pi,
      theta_mod_shift = theta_mod + pi / 2
    ) %>%
    ### Integrate out rho
    group_by(theta_mod_shift) %>%
    summarise(
      score = sum(score)
    )

  ### loess fit
  loess_fit <- loess(score ~ theta_mod_shift, span = loess_span, data = x3p_hough_df_shift)
  ### loess predict
  loess_pred <- predict(loess_fit, x3p_hough_df_shift$theta_mod_shift)

  ### loess cutoff point
  loess_cut <- x3p_hough_df_shift$theta_mod_shift[which.max(loess_pred)]

  ### Filter theta with loess
  theta_filter <- x3p_hough_df_shift %>%
    filter(between(theta_mod_shift, loess_cut - 2 * pi / ntheta, loess_cut + 2 * pi / ntheta))

  ### Filter rho with theta
  x3p_hough_rho_df <- x3p_hough_df %>%
    mutate(
      theta_mod = (theta - pi / 2) %% pi,
      theta_mod_shift = theta_mod + pi / 2
    ) %>%
    filter(
      theta_mod_shift %in% theta_filter$theta_mod_shift
    )

  ### Select main lines
  main_lines <- x3p_hough_rho_df %>%
    filter(score > min_score_cut)

  angles <- (
    (main_lines$theta / pi * 180) %>%
      unique()
    ### 0 is same as pi on the same line
  ) %% 180

  if (ifplot) {
    p <- x3p %>%
      x3p_to_df() %>%
      # filter(!is.na(value)) %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      theme_bw()

    abline_df <- main_lines %>%
      mutate(
        slope = cos(theta) / sin(theta) * x3p$header.info$incrementY / x3p$header.info$incrementX,
        intercept = -rho / sin(theta) * x3p$header.info$incrementY + x3p$header.info$sizeY * x3p$header.info$incrementY,
      ) %>%
      group_by(slope, intercept) %>%
      summarise(
        gg = list(geom_abline(intercept = intercept, slope = slope, col = "red"))
      )

    print(p + abline_df$gg)
  }

  if (ifplot) {
    angle <- ifelse(angles > 90, -(180 - angles), angles) %>%
      mean()

    ### ggplot loess fit
    (x3p_hough_df %>%
      mutate(theta = (theta - pi / 2) %% pi + pi / 2 - pi) %>%
      group_by(theta) %>%
      summarise(
        score = sum(score)
      ) %>%
      ggplot(aes(x = theta, y = score)) +
      geom_point() +
      geom_smooth(method = "loess", span = loess_span) +
      theme_bw() +
      xlab("angle (radians)") +
      geom_vline(xintercept = angle / 180 * pi, col = "red")) %>%
      print()
  }

  return(angles)
}
