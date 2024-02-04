#' Extract signal from raw `x3p` object
#'
#' This function extracts the signal from a raw `x3p` object by computing summary statistics of values along the `y` axis for each `x` value.
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes.
#' @return A data frame with two columns:
#' * x: The `x` values from the `x3p` object.
#' * sig: The extracted signal.
#' @import dplyr
#' @import ggplot2
#' @import x3ptools
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
#' raw_sig_df <- x3p_raw_sig_df(x3p_bin_rotate, ifplot = TRUE)
#'
#' attr(raw_sig_df, "sig_df_plot")
#'
x3p_raw_sig_df <- function(x3p, ifplot = FALSE) {
  assert_that(
    "x3p" %in% class(x3p),
    is.flag(ifplot)
  )

  x <-
    value <-
    sig <-
    y <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  sig_df <- x3p_df %>%
    group_by(x) %>%
    summarise(sig = median(value, na.rm = TRUE)) %>%
    filter(!is.na(sig))

  if (ifplot) {
    p_all <- x3p_df %>%
      ggplot(aes(x = x, y = value)) +
      geom_line(aes(group = y), alpha = 0.1)

    attr(sig_df, "sig_df_plot") <- p_all +
      geom_line(aes(x = x, y = sig), data = sig_df, color = "red") +
      theme_bw()
  }

  return(sig_df)
}
