#' Removing missing values and the quadratic trend from inner polygon
#'
#' Obtained x3p object after removing missing values and the quadratic trend from the inner polygon.
#' @param x3p x3p object
#' @param mask_col colour for the polygon
#' @param concavity strictly positive value used in \code{concaveman::concaveman}
#' @import dplyr
#' @importFrom x3ptools df_to_x3p
#' @importFrom stats lm predict
#' @export

get_x3p_inner_nomiss_res <- function(x3p, mask_col = "#FF0000", concavity = 1.5) {
  x3p_inner_df <- get_x3p_inner_df(x3p, mask_col = mask_col, concavity = concavity)

  x3p_inner_nomiss_df <- x3p_inner_df %>%
    filter(n_neighbor_val_miss == 0)

  ### Remove trend
  x3p_inner_nomiss_lm <- lm(value ~ x + y + I(x^2) + I(y^2) + x:y, data = x3p_inner_nomiss_df)
  x3p_inner_nomiss_res_df <- x3p_inner_nomiss_df %>%
    mutate(value = value - predict(x3p_inner_nomiss_lm, select(., x, y)))

  ### Convert df to x3p
  x3p_inner_nomiss_res <- x3p_inner_nomiss_res_df %>%
    left_join(x3p_inner_df[, c("x", "y")], .) %>%
    df_to_x3p()

  return(x3p_inner_nomiss_res)
}
