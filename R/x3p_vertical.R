#' Rotate imputed x3p object to vertical direction
#'
#' Rotate imputed x3p object with striations to the vertical direction.
#' @param x3p_inner_impute imputed x3p object
#' @param freqs length 4 vector of values corresponding to color frequency (turned into quantiles of the differenced values) used in \code{x3ptools::x3p_bin_stripes}
#' @param method choice of \code{MLE} or \code{quantile} when computing rotation angle
#' @param ntheta number of bins along theta used in \code{imager::hough_line}
#' @param min_score_cut the tuning parameter for minimum scores required in hough transformation
#' @param ifplot whether graphs are displayed
#' @param loess_span the parameter which controls the degree of smoothing, only available when \code{method = MLE}
#' @return x3p object after rotation with vertical striations
#' @import dplyr
#' @importFrom x3ptools x3p_bin_stripes x3p_extract x3p_rotate
#' @export

x3p_vertical <- function(x3p_inner_impute, freqs = c(0, 0.3, 0.7, 1),
                               method = "MLE",
                               ntheta = 720, min_score_cut = 2,
                               ifplot = FALSE,
                               loess_span = 0.2) {
  x3p_bin <- x3p_inner_impute %>%
    x3p_bin_stripes(
      direction = "vertical",
      colors = c("#b12819", "#ffffff", "#134D6B"),
      freqs = freqs
    )

  x3p_bin_red <- x3p_extract(x3p_bin, mask_vals = "#b12819")

  x3p_bin_blue <- x3p_extract(x3p_bin, mask_vals = "#134D6B")

  if (method == "MLE") {
    angle_red <- x3p_MLE_angle_vec(x3p_bin_red, ntheta = ntheta, min_score_cut = min_score_cut, ifplot = ifplot, loess_span = loess_span)
    angle_blue <- x3p_MLE_angle_vec(x3p_bin_blue, ntheta = ntheta, min_score_cut = min_score_cut, ifplot = ifplot, loess_span = loess_span)
  } else {
    if (method == "quantile") {
      angle_red <- x3p_quantile_angle_vec(x3p_bin_red, ntheta = ntheta, min_score_cut = min_score_cut, ifplot = ifplot)
      angle_blue <- x3p_quantile_angle_vec(x3p_bin_blue, ntheta = ntheta, min_score_cut = min_score_cut, ifplot = ifplot)
    } else {
      stop('Not an applicable method, choose from "method = MLE" or "method = quantile"')
    }
  }

  ### Rotation angle theta
  ### Average red and blue angle
  angle <- mean(c(angle_red, angle_blue))
  angle <- ifelse(angle > 90, -(180 - angle), angle)

  angle_red <- mean(angle_red)
  angle_red <- ifelse(angle_red > 90, -(180 - angle_red), angle_red)

  angle_blue <- mean(angle_blue)
  angle_blue <- ifelse(angle_blue > 90, -(180 - angle_blue), angle_blue)

  x3p_bin_red_rotate <- x3p_rotate(x3p_bin_red, angle = angle_red)
  x3p_bin_blue_rotate <- x3p_rotate(x3p_bin_blue, angle = angle_blue)
  x3p_bin_rotate <- x3p_rotate(x3p_bin, angle = angle)

  return(x3p_bin_rotate)
}
