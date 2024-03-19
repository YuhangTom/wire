#' Smooth the signal and remove the hooks
#'
#' This function smooths the signal and removes the hooks by running a t-test in the boundaries to see if there are large 'hooks'.
#'
#' @param sig_df A signal data frame with two columns: `x` and `sig`.
#' @param span1 A positive parameter to control the degree of smoothing in bullterxtrctr::cc_get_signature.
#' @param span2 A positive parameter to control the degree of smoothing in bullterxtrctr::cc_get_signature.
#' @param breaks_q A vector of quantiles to split the `x` values into five groups for t-test of large hooks.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes.
#' @return A data frame with two columns:
#' * x: The `x` values from the `x3p` object.
#' * sig: The extracted signal.
#' @import dplyr
#' @import ggplot2
#' @importFrom bulletxtrctr cc_get_signature
#' @importFrom stats quantile t.test
#' @importFrom assertthat assert_that is.number is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[2]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
#' raw_sig_df <- x3p_raw_sig_df(x3p_bin_rotate, ifplot = FALSE)
#'
#' raw_ccsig_df <- df_ccsig(raw_sig_df, ifplot = TRUE)
#'
#' attr(raw_ccsig_df, "sig_df_plot")
#'
df_ccsig <- function(sig_df, span1 = 400, span2 = 40,
                     breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
                     ifplot = FALSE) {
  assert_that(
    "data.frame" %in% class(sig_df),
    is.number(span1),
    is.number(span2),
    is.vector(breaks_q), near(length(breaks_q), 6),
    is.flag(ifplot)
  )

  x <-
    sig <-
    sig.x <-
    sig.y <-
    smoothed <-
    value <-
    signal <-
    NULL

  raw_sig_df <- sig_df

  dsmall <- sig_df %>%
    select(x, value = sig) %>%
    mutate(y = 1)
  N <- nrow(dsmall)

  resolution <- diff(sort(dsmall$x))[1]
  anchors <- data.frame(x = range(dsmall$x) + resolution * c(-1, 1), value = 0, y = 1)
  dsmall <- dsmall %>% rbind(anchors)

  sigs <- cc_get_signature(dsmall, span1 = span1, span2 = span2)
  sigs <- sigs %>%
    slice_head(n = N) %>%
    arrange(x)

  sigs <- sigs %>% mutate(
    range_x = cut(x,
      breaks = quantile(x, breaks_q),
      include.lowest = TRUE, labels = c("left1", "left2", "middle", "right2", "right1")
    )
  )

  # run a t-test in the boundaries to see if there are large 'hooks'
  left <- t.test(sigs$sig[sigs$range_x == "left1"], sigs$sig[sigs$range_x == "left2"])
  right <- t.test(sigs$sig[sigs$range_x == "right1"], sigs$sig[sigs$range_x == "right2"])

  if (left$p.value < 0.01) sigs$sig[sigs$range_x == "left1"] <- NA
  if (right$p.value < 0.01) sigs$sig[sigs$range_x == "right1"] <- NA

  sig_df$sig <- sigs$sig
  sig_df <- sig_df %>%
    filter(!is.na(sig))

  if (ifplot) {
    sig_plot_df <- full_join(sig_df, raw_sig_df, by = "x") %>%
      rename(smoothed = sig.x, raw = sig.y) %>%
      pivot_longer(smoothed:raw, names_to = "signal", values_to = "value")

    attr(sig_df, "sig_df_plot") <- sig_plot_df %>%
      ggplot(aes(x = x, y = value, color = signal)) +
      geom_line() +
      scale_colour_brewer(palette = "Paired") +
      xlab(expression(paste("x (", mu, "m)"))) +
      ylab(expression(paste("signal value (", mu, "m)"))) +
      theme_bw()
  }

  return(sig_df)
}
