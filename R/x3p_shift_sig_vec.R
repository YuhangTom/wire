#' Extract signal with transformed `x3p` object by minimizing MSE
#'
#' Extract signal by computing summary statistics of values along `y` for each `x` with transformed `x3p` object by minimizing MSE.
#' @param x3p `x3p` object
#' @param method choice of `median` or `mean` when computing the summary statistics
#' @param ifplot whether graphs are displayed
#' @param delta shifting range when minimizing MSE
#' @return data frame of 2 columns
#' * x: x value
#' * sig: signal extracted
#' @import dplyr
#' @importFrom x3ptools x3p_to_df x3p_delete_mask x3p_bin_stripes
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom stats na.omit median lm coef approx
#' @importFrom purrr map_dbl map set_names
#' @importFrom tidyr nest unnest
#' @importFrom assertthat assert_that is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[2]]
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
#'
#' if (interactive()) {
#'   x3p_shift_sig_vec(x3p_bin_rotate, ifplot = TRUE) %>%
#'     str()
#' }
#'
x3p_shift_sig_vec <- function(x3p, method = "median", ifplot = FALSE, delta = -5:5) {
  assert_that(
    "x3p" %in% class(x3p),
    method %in% c("median", "mean"),
    is.flag(ifplot),
    is.numeric(delta), length(delta) >= 3
  )

  x3p_approx <- x3p_shift(x3p, ifplot = ifplot, delta = delta)
  shift_sig <- x3p_raw_sig_vec(x3p_approx, method = method, ifplot = ifplot)

  return(shift_sig)
}
