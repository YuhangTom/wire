#' Extract signal from raw `x3p` object
#'
#' This function extracts the signal from a raw `x3p` object by computing summary statistics of values along the `y` axis for each `x` value.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param method A string indicating the method for computing summary statistics. Options are `median` or `mean`.
#' @param ifplot A Boolean flag indicating whether to display graphs.
#' @return A data frame with two columns:
#' * x: The `x` values from the `x3p` object.
#' * sig: The extracted signal.
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
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
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
    y <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  raw_sig <- x3p_df %>%
    group_by(x) %>%
    summarise(sig = ifelse(method == "median", median(value, na.rm = TRUE),
      mean(value, na.rm = TRUE)
    ))

  if (ifplot) {
    p_all <- x3p_df %>%
      ggplot(aes(x = x, y = value)) +
      geom_line(aes(group = y), alpha = 0.1)

    (p_all +
      geom_line(aes(x = x, y = sig), data = raw_sig, color = "red")) %>%
      print()
  }

  return(raw_sig)
}
