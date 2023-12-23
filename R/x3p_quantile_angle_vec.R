#' Calculate rotation angle using quantile method
#'
#' This function calculates the rotation angle of an `x3p` object using the quantile method with Hough transformation.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ntheta An integer representing the number of bins along the theta axis used in `imager::hough_line`.
#' @param min_score_cut A numeric value representing the minimum score required in the Hough transformation.
#' @param ifplot A Boolean flag indicating whether to display graphs.
#' @return A vector of rotation angles computed by the quantile procedure.
#' @import dplyr
#' @importFrom ggplot2 ggplot aes geom_histogram geom_point
#' @importFrom raster raster
#' @importFrom imager as.cimg hough_line nfline
#' @importFrom stats quantile median
#' @importFrom assertthat assert_that is.count is.number is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#' ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
#'
#' x3p_quantile_angle_vec(x3p_inner_impute, min_score_cut = 0.1, ifplot = TRUE) %>%
#'     str()
#'
x3p_quantile_angle_vec <- function(x3p, ntheta = 720, min_score_cut = 0.1,
                                          ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    is.count(ntheta),
    is.number(min_score_cut),
    is.flag(ifplot)
  )

  theta <-
    theta_mod <-
    theta_mod_shift <-
    score <-
    rho <-
    . <-
    NULL

  ### Change to contrast color
  x3p_shift <- x3p$surface.matrix
  x3p_shift[is.na(x3p$surface.matrix)] <-
    -(x3p$surface.matrix %>%
      c() %>%
      summary() %>%
      .[c("Min.", "Max.")] %>%
      abs() %>%
      max() %>%
      ceiling(.))

  ### Change to raster
  x3p_raster <- t(x3p_shift) %>%
    raster(xmx = x3p$header.info$sizeX - 1, ymx = x3p$header.info$sizeY - 1)

  ### Change to cimg
  x3p_cimg <- as.cimg(x3p_raster)

  if (ifplot) {
    plot(x3p_cimg)
  }

  ### Hough transformation for lines
  x3p_hough_df <- hough_line(x3p_cimg, ntheta = ntheta, data.frame = TRUE, shift = FALSE)

  ### For theta_mod: 0 = pi = 2 * pi <- pi / 2, pi / 2 = pi * 3 / 2 <- 0
  ### For theta_mod_shift: 0 = pi = 2 * pi <- pi, pi / 2 = pi * 3 / 2 <- pi / 2
  ### For theta_mod_shift / pi: 0 = pi = 2 * pi <- 1, pi / 2 = pi * 3 / 2 <- 1 / 2
  x3p_hough_df_shift <- x3p_hough_df %>%
    mutate(
      theta_mod = (theta - pi / 2) %% pi,
      theta_mod_shift = theta_mod + pi / 2
    )

  if (ifplot) {
    (x3p_hough_df_shift %>%
      ggplot(aes(x = theta_mod_shift / pi, weight = score)) +
      geom_histogram()) %>%
      print()
  }

  if (ifplot) {
    (x3p_hough_df_shift %>%
      ggplot(aes(x = theta_mod_shift / pi, y = score)) +
      geom_point()) %>%
      print()
  }

  ### How to set a score cutoff line without hardcoding the quantile?
  theta_mod_shift_med <- x3p_hough_df_shift %>%
    filter(score >= quantile(.data$score, 0.99995, na.rm = TRUE)) %>%
    summarise(med = median(theta_mod_shift)) %>%
    unlist()

  if (ifplot) {
    ### Get theta near the median of theta with high scores in a very small range
    ### Divide by ntheta as hough_line cut whole circle into ntheta tiny intervals
    (x3p_hough_df_shift %>%
      filter(between(theta_mod_shift, theta_mod_shift_med - 2 * pi / ntheta, theta_mod_shift_med + 2 * pi / ntheta)) %>%
      ggplot(aes(x = rho, weight = score)) +
      geom_histogram()) %>%
      print()
  }

  ### How to set a score cutoff line without hardcoding?
  main_lines <- x3p_hough_df_shift %>%
    filter(between(theta_mod_shift, theta_mod_shift_med - 2 * pi / ntheta, theta_mod_shift_med + 2 * pi / ntheta)) %>%
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
      geom_histogram(aes(x = abs(rho), weight = score), data = main_lines)) %>%
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
