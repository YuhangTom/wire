#' Extract signal with raw x3p object
#'
#' Extract signal by computing summary statistics of values along y for each x with raw x3p object.
#' @param x3p x3p object
#' @param method choice of \code{median} or \code{mean} when computing the summary statistics
#' @param ifplot whether graphs are displayed
#' @import dplyr x3ptools ggplot2
#' @importFrom stats na.omit median
#' @export

get_sig_raw <- function(x3p, method = "median", ifplot = FALSE) {
  x3p_df <- x3p %>%
    x3p_to_df()

  sig <- x3p_df %>%
    na.omit() %>%
    group_by(x) %>%
    summarise(value_summ = ifelse(method == "median", median(value, na.rm = TRUE),
      ifelse(method == "mean", mean(value, na.rm = TRUE),
        stop('Not an applicable method, choose from "method = median" or "method = mean"')
      )
    ))

  if (ifplot) {
    (sig %>%
      ggplot(aes(x = x, y = value_summ)) +
      geom_line()) %>%
      print()
  }

  return(sig)
}
