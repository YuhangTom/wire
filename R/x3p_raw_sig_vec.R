#' Extract signal with raw x3p object
#'
#' Extract signal by computing summary statistics of values along y for each x with raw x3p object.
#' @param x3p x3p object
#' @param method choice of \code{median} or \code{mean} when computing the summary statistics
#' @param ifplot whether graphs are displayed
#' @return vector of raw signal extracted
#' @import dplyr
#' @importFrom x3ptools x3p_to_df
#' @importFrom ggplot2 ggplot aes geom_line
#' @importFrom stats na.omit median
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' mask_col <- "#FF0000"
#' concavity <- 1.5
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = mask_col, concavity = concavity)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, x3p, mask_col = mask_col,
#' concavity = concavity, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
#'
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1)
#' x3p_raw_sig_vec(x3p_bin_rotate) %>%
#' str()
#'
x3p_raw_sig_vec <- function(x3p, method = "median", ifplot = FALSE) {
  x <-
    value <-
    value_summary <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  sig <- x3p_df %>%
    na.omit() %>%
    group_by(x) %>%
    summarise(value_summary = ifelse(method == "median", median(value, na.rm = TRUE),
      ifelse(method == "mean", mean(value, na.rm = TRUE),
        stop('Not an applicable method, choose from "method = median" or "method = mean"')
      )
    ))

  if (ifplot) {
    (sig %>%
      ggplot(aes(x = x, y = value_summary)) +
      geom_line()) %>%
      print()
  }

  return(sig$value_summary)
}
