#' Align signals
#'
#' Align signals with plot.
#' @param sig1 first numeric signal vector
#' @param sig2 second numeric signal vector
#' @param min.overlap additional parameter passed on to `bulletxtrctr::get_ccf`
#' @param ifplot whether graphs are displayed
#' @param name1 name for the first cut
#' @param name2 name for the second cut
#' @param legendname legend name
#' @param titlename title name
#' @return list of aligned signals named `ccf`, `lag` and `lands` followed the output format of `bulletxtrctr::sig_align`
#' @importFrom ggplot2 ggplot aes geom_line labs xlab ylab ggtitle
#' @importFrom bulletxtrctr sig_align
#' @importFrom assertthat assert_that not_empty is.count is.flag is.string
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#' ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1)
#'
#' vec_align_sigs_list(x3p_raw_sig_vec(x3p_bin_rotate), x3p_shift_sig_vec(x3p_bin_rotate),
#' ifplot = TRUE) %>%
#' str()
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
    is.count(min.overlap)
  }
  if (not_empty(titlename)) {
    is.string(titlename)
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
