#' Compute the rotation angle using MLE
#'
#' Compute the rotation angle using MLE with hough transformation.
#' @param x3p x3p object
#' @param ntheta number of bins along theta used in \code{imager::hough_line}
#' @param min_score_cut the tuning parameter for minimum scores required in hough transformation
#' @param ifplot whether graphs are displayed
#' @param loess_span the parameter which controls the degree of smoothing
#' @import dplyr ggplot2
#' @importFrom raster raster
#' @importFrom imager as.cimg hough_line nfline
#' @importFrom stats loess predict
#' @export

get_x3p_rotate_angle_MLE <- function(x3p, ntheta = 720, min_score_cut = 2,
                                     ifplot = FALSE,
                                     loess_span = 0.2) {
  ### Change to contrast color
  x3p_shift <- x3p$surface.matrix
  NA_val <- -(x3p$surface.matrix %>%
    c() %>%
    summary() %>%
    .[c("Min.", "Max.")] %>%
    abs() %>%
    max() %>%
    ceiling())
  x3p_shift[is.na(x3p$surface.matrix)] <- NA_val

  ### Change to raster
  x3p_raster <- t(x3p_shift) %>%
    raster(xmx = (x3p$header.info$sizeX - 1) * x3p$header.info$incrementX, ymx = (x3p$header.info$sizeY - 1) * x3p$header.info$incrementY)

  ### Change to cimg
  x3p_cimg <- as.cimg(x3p_raster)

  if (ifplot) {
    plot(x3p_cimg)
  }

  ### Hough transformation for lines
  x3p_hough_df <- hough_line(x3p_cimg, ntheta = ntheta, data.frame = TRUE, shift = FALSE)

  if (ifplot) {
    ### Histogram with bins
    (x3p_hough_df %>%
      ggplot(aes(x = theta, weight = score)) +
      geom_histogram(bins = ntheta)) %>%
      print()
  }

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

  if (ifplot) {
    ### ggplot loess fit
    (x3p_hough_df_shift %>%
      ggplot(aes(x = theta_mod_shift / pi, y = score)) +
      geom_point() +
      geom_smooth(method = "loess", span = loess_span)) %>%
      print()
  }

  ### loess fit
  loess_fit <- loess(score ~ theta_mod_shift, span = loess_span, data = x3p_hough_df_shift)
  ### loess predict
  loess_pred <- predict(loess_fit, x3p_hough_df_shift$theta_mod_shift)

  if (ifplot) {
    ### loess fit
    (x3p_hough_df_shift %>%
      mutate(loess_pred = loess_pred) %>%
      ggplot(aes(x = theta_mod_shift, y = loess_pred)) +
      geom_point()) %>%
      print()
  }

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

  if (ifplot) {
    ### Histogram with rho
    ### Ideally should look like our strikes (signals), with rotated x axis
    (x3p_hough_rho_df %>%
      filter(score > min_score_cut) %>%
      ggplot(aes(x = rho, weight = score)) +
      geom_histogram(bins = 150)) %>%
      print()
  }

  ### Select main lines
  main_lines <- x3p_hough_rho_df %>%
    filter(score > min_score_cut)

  if (ifplot) {
    ### Plot image with main lines after selecting theta
    plot(x3p_cimg)
    with(main_lines, nfline(theta, rho, col = "red"))
  }

  if (ifplot) {
    ### Distribution of transformed theta and absolute value of rho
    (main_lines %>%
      ggplot(aes(x = theta_mod_shift / pi, weight = score)) +
      geom_histogram() +
      main_lines %>%
      ggplot(aes(x = abs(rho), weight = score)) +
      geom_histogram()) %>%
      print()
  }

  return(
    (
      (main_lines$theta / pi * 180) %>%
        unique()
      ### 0 is same as pi on the same line
    ) %% 180
  )
}
