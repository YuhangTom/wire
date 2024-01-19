#' Rotate imputed `x3p` object to vertical direction
#'
#' This function rotates an imputed `x3p` object with striations to the vertical direction.
#'
#' @param x3p_inner_impute An `x3p` object after imputation.
#' @param freqs A numeric vector of length 4, representing color frequency quantiles used in `x3ptools::x3p_bin_stripes`.
#' @param method A string indicating the method for computing rotation angle. Options are `MLE` or `quantile`.
#' @param ntheta An integer representing the number of bins along the theta axis used in `imager::hough_line`.
#' @param min_score_cut A numeric value representing the minimum score required in the Hough transformation.
#' @param ifplot A Boolean flag indicating whether to display graphs.
#' @param loess_span A numeric value controlling the degree of smoothing. This is only available when `method = MLE`.
#' @return An `x3p` object after rotation with vertical striations.
#' @import dplyr
#' @importFrom x3ptools x3p_bin_stripes x3p_extract x3p_rotate
#' @importFrom assertthat assert_that is.count is.number is.flag is.number
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#'
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 5, ifplot = TRUE)
#' x3p_bin_rotate
#' if (interactive()) {
#'   x3p_image_autosize(x3p_bin_rotate)
#' }
#'
x3p_vertical <- function(x3p_inner_impute, freqs = c(0, 0.3, 0.7, 1),
                         method = "MLE",
                         ntheta = 720, min_score_cut = 0.1,
                         ifplot = FALSE,
                         loess_span = 0.2) {
  assert_that(
    "x3p" %in% class(x3p_inner_impute),
    is.numeric(freqs), near(length(freqs), 4), near(freqs[1], 0), near(freqs[4], 1),
    method %in% c("MLE", "quantile"),
    is.count(ntheta),
    is.number(min_score_cut),
    is.flag(ifplot)
  )

  if (method == "MLE") {
    assert_that(
      is.number(loess_span), loess_span > 0
    )
  }
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
    }
  }

  ### Rotation angle theta
  ### Average red and blue angle
  angles <- c(angle_red, angle_blue)
  angles <- ifelse(angles > 90, -(180 - angles), angles)
  angle <- mean(angles)
  if (sd(angles) > 10) {
    warning("more than 10 degrees of variation in angles. cause for concern???")
  }
  if (abs(diff(range(angles))) >= 90) {
    warning("more than 90 degrees of range in angles. cause for concern???")
  }

  # angle_red <- mean(angle_red)
  # angle_red <- ifelse(angle_red > 90, -(180 - angle_red), angle_red)

  # angle_blue <- mean(angle_blue)
  # angle_blue <- ifelse(angle_blue > 90, -(180 - angle_blue), angle_blue)

  #  x3p_bin_red_rotate <- x3p_rotate(x3p_bin_red, angle = angle_red)
  #  x3p_bin_blue_rotate <- x3p_rotate(x3p_bin_blue, angle = angle_blue)
  x3p_bin_rotate <- x3p_rotate(x3p_bin, angle = angle)

  return(x3p_bin_rotate)
}
