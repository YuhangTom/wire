#' Align two signal vectors
#'
#' This function aligns two numeric signal vectors. It also provides an option to visualize the aligned signals.
#'
#' @param sig1 The first numeric signal vector.
#' @param sig2 The second numeric signal vector.
#' @param min.overlap An optional parameter passed to `bulletxtrctr::get_ccf` to specify the minimum overlap between signals.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes.
#' @param legendname A string to label the legend in the plot.
#' @param titlename A string to set the title of the plot.
#' @param subtitlename A string to set the subtitle of the plot. Show `ccf` and `lag` if `TRUE`.
#' @return A list containing the cross-correlation function (`ccf`), the lag (`lag`), and the landmarks (`lands`) of the aligned signals. This follows the output format of `bulletxtrctr::sig_align`.
#' @import ggplot2
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
#' aligned <- vec_align_sigs_list(
#'   x3p_raw_sig_vec(x3p_bin_rotate)$sig,
#'   x3p_shift_sig_vec(x3p_bin_rotate)$sig,
#'   ifplot = TRUE,
#'   subtitlename = TRUE
#' )
#'
#' attr(aligned, "sig_align_plot")
#'
vec_align_sigs_list <- function(
    sig1,
    sig2,
    min.overlap = NULL,
    ifplot = FALSE,
    legendname = "Signal",
    titlename = NULL,
    subtitlename = TRUE) {
  assert_that(
    is.numeric(sig1),
    is.numeric(sig2),
    is.flag(ifplot),
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

  x <-
    value <-
    NULL

  aligned <- sig_align(sig1, sig2, min.overlap = min.overlap)

  if (!is.null(subtitlename)) {
    subtitlename <- ifelse(isTRUE(subtitlename), paste0("ccf = ", round(aligned$ccf, 3), "; lag = ", aligned$lag), as.character(subtitlename))
  }

  if (ifplot) {
    attr(aligned, "sig_align_plot") <- aligned$lands %>%
      pivot_longer(sig1:sig2, names_to = legendname, names_prefix = "sig") %>%
      ggplot(aes(x = x, y = value)) +
      geom_line(aes(colour = !!sym(legendname))) +
      theme_bw() +
      scale_colour_brewer(palette = "Paired") +
      xlab("x") +
      ylab("signal value") +
      ggtitle(titlename, subtitle = subtitlename)
  }

  aligned
}
