#' Removing missing values and the quadratic trend from inner polygon
#'
#' Obtained x3p object after removing missing values and the quadratic trend from the inner polygon.
#' @param insidepoly_df data frame of inside polygon
#' @return x3p object of residuals after removing trend
#' @import dplyr
#' @importFrom x3ptools df_to_x3p
#' @importFrom stats lm predict
#' @export
#' @examples
#' x3p <- x3p_subsamples[[1]]
#' mask_col <- "#FF0000"
#' concavity <- 1.5
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = mask_col, concavity = concavity, b = 1)
#'
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_nomiss_res
#'
#' if (interactive()) {
#'   x3p_image_autosize(x3p_inner_nomiss_res)
#' }
#'
df_rmtrend_x3p <- function(insidepoly_df) {
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
  x3p_inner_nomiss_res <- x3p_add_mask(x3p_inner_nomiss_res, mask = x3p_insidepoly_mask)

  return(x3p_inner_nomiss_res)
}
