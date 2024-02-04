#' Removing missing values and a quadratic trend
#'
#' This function takes an `x3p` object, specifically the inner polygon data frame, and performs two main operations:
#' 1. Removes missing values.
#' 2. Detrends the data by removing a quadratic trend.
#'
#' @param insidepoly_df A data frame representing the inner polygon. This is typically obtained from the `wire::x3p_insidepoly_df` function.
#' @return An `x3p` object that contains the residuals after the removal of the quadratic trend.
#' @import dplyr
#' @import x3ptools
#' @importFrom stats lm predict
#' @importFrom assertthat assert_that has_name
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#'
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_nomiss_res
#' if (interactive()) {
#'   x3p_image_autosize(x3p_inner_nomiss_res)
#' }
#'
df_rmtrend_x3p <- function(insidepoly_df) {
  assert_that(
    is.data.frame(insidepoly_df),
    has_name(insidepoly_df, c("x", "y", "value", "mask", "n_neighbor_val_miss"))
  )

  n_neighbor_val_miss <-
    value <-
    x <-
    y <-
    . <-
    NULL

  x3p_inner_nomiss_df <- insidepoly_df %>%
    filter(n_neighbor_val_miss == 0)

  ### Remove trend
  x3p_inner_nomiss_lm <- lm(value ~ x + y + I(x^2) + I(y^2) + x:y, data = x3p_inner_nomiss_df)
  x3p_inner_nomiss_res_df <- x3p_inner_nomiss_df %>%
    mutate(value = value - predict(x3p_inner_nomiss_lm, select(., x, y)))

  ### Convert df to x3p
  x3p_inner_nomiss_res <- x3p_inner_nomiss_res_df %>%
    left_join(insidepoly_df[, c("x", "y")], ., by = join_by(x, y)) %>%
    df_to_x3p()

  ### Change mask
  x3p_insidepoly_mask <- insidepoly_df %>%
   df_to_x3p() %>%
    .$mask
  x3p_inner_nomiss_res <- x3p_add_mask(x3p_inner_nomiss_res, mask = x3p_insidepoly_mask) %>%
    x3p_trim_na(ratio = 1)

  return(x3p_inner_nomiss_res)
}
