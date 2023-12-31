#' Align two signal vectors
#'
#' This function aligns two numeric signal vectors. It also provides an option to visualize the aligned signals.
#'
#' @param sig1 The first numeric signal vector.
#' @param sig2 The second numeric signal vector.
#' @param min.overlap An optional parameter passed to `bulletxtrctr::get_ccf` to specify the minimum overlap between signals.
#' @param ifplot A Boolean flag indicating whether to plot the aligned signals.
#' @param name1 A string to label the first signal in the plot.
#' @param name2 A string to label the second signal in the plot.
#' @param legendname A string to label the legend in the plot.
#' @param titlename A string to set the title of the plot.
#' @return A list containing the cross-correlation function (`ccf`), the lag (`lag`), and the landmarks (`lands`) of the aligned signals. This follows the output format of `bulletxtrctr::sig_align`.
#' @importFrom ggplot2 ggplot aes geom_line labs xlab ylab ggtitle
#' @importFrom bulletxtrctr sig_align
#' @importFrom assertthat assert_that not_empty is.count is.flag is.string
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1)
#'
#' vec_align_sigs_list(x3p_raw_sig_vec(x3p_bin_rotate)$sig, x3p_shift_sig_vec(x3p_bin_rotate)$sig,
#'   ifplot = TRUE
#' ) %>%
#'   str()
#'
vec_align_sigs_list <- function(
    sig1,
    sig2,
    min.overlap = NULL,
    ifplot = FALSE,
    name1 = "Cut1",
    name2 = "Cut2",
    legendname = "Signal",
    titlename = NULL) {
  assert_that(
    is.numeric(sig1),
    is.numeric(sig2),
    is.flag(ifplot),
    is.string(name1),
    is.string(name2),
    is.string(legendname)
  )
  if (not_empty(min.overlap)) {
    assert_that(
      is.count(min.overlap)
    )
  }
  if (not_empty(titlename)) {
    assert_that(
      is.string(titlename)
    )
  }

  x <- NULL

  sigalign <- sig_align(sig1, sig2)
  if (ifplot) {
    p <- sigalign$lands %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = sig1, color = name1)) +
      geom_line(aes(y = sig2, color = name2)) +
      labs(color = legendname) +
      xlab("x") +
      ylab("sig") +
      ggtitle(titlename, subtitle = paste0("ccf = ", round(sigalign$ccf, 3), "; lag = ", sigalign$lag))
    print(p)
  }

  sigalign
}
