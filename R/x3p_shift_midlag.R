#' Shift striations by minimizing MSE
#'
#' Shift striations on `x3p` object by minimizing MSE.
#' @param x3p `x3p` object
#' @param ifplot whether graphs are displayed
#' @param delta shifting range when minimizing MSE
#' @param delta_q_range lower and upper bound for quantile taken
#' @return `x3p` object after transformation
#' @import dplyr
#' @importFrom x3ptools x3p_to_df x3p_delete_mask x3p_bin_stripes
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom stats lm coef approx
#' @importFrom purrr map_dbl map set_names
#' @importFrom tidyr nest unnest
#' @importFrom assertthat assert_that is.flag
#' @export
#' @examples
#' x3p <- x3p_subsamples[[2]]
#'
#' insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
#' x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
#' x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
#'   ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
#' )
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
#'
#' if (interactive()) {
#'   x3p_shift_midlag(x3p_bin_rotate, ifplot = TRUE)
#' }
#'
x3p_shift_midlag <- function(x3p, ifplot = FALSE, delta = -5:5,
                             delta_q_range = c(0.1, 0.9)) {
  assert_that(
    "x3p" %in% class(x3p),
    is.flag(ifplot),
    is.numeric(delta), length(delta) >= 3
  )

  y <-
    value_nobs <-
    x <-
    value <-
    x_shift_delta_value <-
    x_shift_delta <-
    Dat <-
    value_approx <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  scale <- x3p_get_scale(x3p)
  ### reverse column id for surface matrix with more than 1 observed value
  yidx <- rev(which(colSums(!is.na(x3p$surface.matrix)) >= 2))
  ### all possible y
  y_sort <- x3p_df$y %>%
    unique() %>%
    sort()
  y_sort <- y_sort[rev(yidx)]

  ### middle column id for surface matrix with more than 1 observed value
  yidx_mid <- yidx[floor(length(yidx) / 2)]
  y_sort_mid <- y_sort[floor(length(yidx) / 2)]

  delta_min <- (1:length(yidx)) %>%
    map_dbl(function(j) {
      if (near(j, floor(length(yidx) / 2))) {
        0
      } else {
        ### f1 values
        f1 <- x3p$surface.matrix[, yidx_mid]
        f2 <- x3p$surface.matrix[, yidx[j]]

        ### Mean squared error for all delta
        MSE <- map_dbl(delta, function(delta_i) {
          ### Too few non-missing values, cannot do anything
          if (sum(!is.na(f2)) < 30) {
            return(NA)
          }

          if (delta_i == 0) {
            mean((f1 - f2)^2, na.rm = TRUE)
          } else {
            if (delta_i > 0) {
              mean((f1[-(1:delta_i)] - f2[1:(length(f2) - delta_i)])^2, na.rm = TRUE)
            } else {
              mean((f1[1:(length(f1) - abs(delta_i))] - f2[-(1:abs(delta_i))])^2, na.rm = TRUE)
            }
          }
        }) %>%
          set_names(delta)

        ### Fit parabola
        if (near(sum(is.na(MSE)), length(MSE))) {
          NA
        } else {
          para_coef <- lm(MSE ~ delta + I(delta^2)) %>%
            coef()

          ### Get delta with minimum mean squared error
          out <- (-para_coef[2] / (2 * para_coef[3])) %>%
            unname()

          ### Minimum value of parabola is far from delta with minmimum MSE
          ### Bad fit
          if (abs(delta[which.min(MSE)] - out) > max(delta) - min(delta)) {
            warning("Minimum value of the parabola is too far away from the delta with minimum MSE. Use 0 shifting.")

            out <- 0

            # out <- pmin(out, max(delta))
            # out <- pmax(out, min(delta))
          }

          out
        }
      }
    })

  if (ifplot) {
    p_delta <- data.frame(
      y_sort = y_sort,
      delta_min = delta_min
    ) %>%
      ggplot(aes(x = y_sort, y = delta_min)) +
      geom_line()
    print(p_delta)
  }

  ### Tuning parameter
  delta_min_quantile <- delta_min %>%
    quantile(delta_q_range, na.rm = TRUE)

  if (delta_min_quantile[1] > 0) {
    warning(paste0("0 is smaller than Q1 = ", round(delta_min_quantile[1], 4), " in shifting values. The lower bound is set to 0."))
    delta_min_quantile[1] <- 0
  } else {
    if (delta_min_quantile[2] < 0) {
      warning(paste0("0 is larger than Q3 = ", round(delta_min_quantile[2], 4), " in shifting values. The upper bound is set to 0"))
      delta_min_quantile[2] <- 0
    }
  }

  ### Combine with y values
  x_shift_delta_value_df <- data.frame(
    ### Fix x curve with largest y
    y = y_sort,
    delta_min = delta_min,
    ### Shift x by increment in x
    x_shift_delta_value = delta_min * x3p$header.info$incrementX
  ) %>%
    filter(between(delta_min, delta_min_quantile[1], delta_min_quantile[2]))

  length_bigger <- which(y_sort_mid <= x_shift_delta_value_df$y) %>%
    length()
  length_smaller <- which(y_sort_mid > x_shift_delta_value_df$y) %>%
    length()
  x_shift_delta_value_df <- x_shift_delta_value_df %>%
    filter(
      near(
        c(
          seq(to = y_sort_mid - scale, length.out = length_smaller, by = scale),
          seq(from = y_sort_mid, length.out = length_bigger, by = scale)
        ), x_shift_delta_value_df$y
      )
    )

  ### Shift x values
  x3p_shift_delta_df <- inner_join(x3p_df, x_shift_delta_value_df, by = join_by(y)) %>%
    mutate(x_shift_delta = x + x_shift_delta_value)

  if (ifplot) {
    (x3p_shift_delta_df %>%
      ggplot(aes(x = x, y = y, color = value)) +
      geom_point()) %>%
      print()
  }

  if (ifplot) {
    (x3p_shift_delta_df %>%
      ggplot(aes(x = x_shift_delta, y = y, color = value)) +
      geom_point()) %>%
      print()
  }

  # shift scan horizontally to let it start in 0
  x3p_shift_delta_df <- x3p_shift_delta_df %>%
    filter(!is.na(value)) %>%
    mutate(
      x_shift_delta = x_shift_delta - min(x_shift_delta, na.rm = TRUE)
    )

  # switch from scale resolution to (approximate) integer resolution
  x3p_shift_delta_df <- x3p_shift_delta_df %>%
    mutate(
      x_shift_delta = x_shift_delta / scale
    )

  approx_range <- seq(0, ceiling(max(x3p_shift_delta_df$x_shift_delta, na.rm = TRUE)))

  # approximate on integer resolution
  x3p_approx_df <- x3p_shift_delta_df %>%
    group_by(y) %>%
    nest(.key = "Dat") %>%
    mutate(Dat = Dat %>% map(.f = function(dat) {
      if ((sum(!is.na(dat$x_shift_delta)) < 2) || (sum(!is.na(dat$value)) < 2)) {
        dat$value_approx <- NA
      } # can't do anything
      else {
        dat <- approx(x = dat$x_shift_delta, y = dat$value, xout = approx_range) %>%
          as.data.frame() %>%
          rename(x = x, value_approx = y)
      }
      dat
    })) %>%
    unnest(Dat) %>%
    select(x, y, value_approx)

  # scale back to resolution <scale>
  x3p_approx_df <- x3p_approx_df %>%
    mutate(
      x = x * scale
    )

  if (ifplot) {
    (x3p_approx_df %>%
      ggplot(aes(x = x, y = y, color = value_approx)) +
      geom_point()) %>%
      print()
  }

  x3p_approx <- x3p_approx_df %>%
    df_to_x3p(var = "value_approx") %>%
    x3p_bin_stripes(
      direction = "vertical",
      colors = c("#b12819", "#ffffff", "#134D6B"),
      freqs = c(0, 0.3, 0.7, 1)
    )

  if (ifplot) {
    x3p %>%
      x3p_image_autosize(ifhtml = TRUE)

    x3p_approx %>%
      x3p_image_autosize(ifhtml = TRUE)
  }

  return(x3p_approx)
}
