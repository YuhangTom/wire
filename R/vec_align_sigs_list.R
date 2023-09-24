#' Align signals
#'
#' Align signals with plot.
#' @param sig1 first signal vector
#' @param sig2 second signal vector
#' @param min.overlap additional parameter passed on to \code{bulletxtrctr::get_ccf}
#' @param ifplot whether graphs are displayed
#' @param name1 name for the first cut
#' @param name2 name for the second cut
#' @param legendname legend name
#' @param titlename title name
#' @return list of aligned signals named \code{ccf}, \code{lag} and \code{lands} followed the output format of \code{bulletxtrctr::sig_align}
#' @importFrom ggplot2 ggplot aes geom_line labs xlab ggtitle
#' @importFrom bulletxtrctr sig_align
#' @export

vec_align_sigs_list <- function(
    sig1,
    sig2,
    min.overlap = round(0.75 * min(length(sig1), length(sig2))),
    ifplot = FALSE,
    name1 = "Cut1",
    name2 = "Cut2",
    legendname = "Signal",
    titlename = NULL) {
  x <- NULL

  sigalign <- sig_align(sig1, sig2)
  if (ifplot) {
    p <- sigalign$lands %>%
      ggplot(aes(x = x)) +
      geom_line(aes(y = sig1, color = name1)) +
      geom_line(aes(y = sig2, color = name2)) +
      labs(color = legendname) +
      xlab("x") +
      ggtitle(titlename)
    print(p)
  }

  sigalign
}
