#' Extract signal with raw `x3p` object
#'
#' Extract signal by computing summary statistics of values along `y` for each `x` with raw `x3p` object.
#' @param x3p `x3p` object
#' @param method choice of `median` or `mean` when computing the summary statistics
#' @param ifplot whether graphs are displayed
#' @return data frame of 2 columns
#' * x: x value
#' * sig: signal extracted
#' @import dplyr
#' @importFrom x3ptools x3p_to_df
#' @importFrom ggplot2 ggplot aes geom_line
#' @importFrom stats na.omit median
#' @importFrom assertthat assert_that is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[2]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
#'
#' x3p_raw_sig_vec(x3p_bin_rotate, ifplot = TRUE) %>%
#'   str()
#'
x3p_raw_sig_vec <- function(x3p, method = "median", ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    method %in% c("median", "mean"),
    is.flag(ifplot)
  )

  x <-
    value <-
    sig <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  raw_sig <- x3p_df %>%
    na.omit() %>%
    group_by(x) %>%
    summarise(sig = ifelse(method == "median", median(value, na.rm = TRUE),
      mean(value, na.rm = TRUE)
    ))

  if (ifplot) {
    (raw_sig %>%
      ggplot(aes(x = x, y = sig)) +
      geom_line()) %>%
      print()
  }

  return(raw_sig)
}