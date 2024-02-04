#' Minimize MSE by shifting striations
#'
#' This function shifts the striations on an `x3p` object to minimize the Mean Squared Error (MSE).
#'
#' @param x3p An `x3p` object representing a topographic scan.
#' @param ifplot A Boolean flag indicating whether to save ggplot lists in the output attributes.
#' @param delta A numeric vector representing the shifting range for minimizing MSE.
#' @param delta_q_range A numeric vector of length 2, representing the lower and upper bounds for the quantile taken.
#' @return An `x3p` object after the transformation.
#' @import dplyr
#' @import ggplot2
#' @import x3ptools
#' @importFrom stats lm coef approx quantile
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
#'   x3p_approx <- x3p_shift(x3p_bin_rotate, ifplot = TRUE)
#'
#'   attr(x3p_approx, "x3p_before_shift_plot")
#'   attr(x3p_approx, "x3p_after_shift_plot")
#'   attr(x3p_approx, "MSE_plot")
#' }
#'
x3p_shift <- function(x3p, ifplot = FALSE, delta = -5:5,
                      delta_q_range = c(0, 1)) {
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

  ggplot_attrs <- NA

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
        if (length(MSE) - sum(is.na(MSE)) < 3) {
          warning("No enough non-NA MSE values to fit parabola.")

          return(NA)
        } else {
          para_coef <- lm(MSE ~ delta + I(delta^2)) %>%
            coef()

          ### Get delta with minimum mean squared error
          out <- (-para_coef["delta"] / (2 * para_coef["I(delta^2)"])) %>%
            unname()

          ### Consider different a values
          if (para_coef["I(delta^2)"] < 0) {
            if (out >= 0) {
              out <- (-para_coef["delta"] + sqrt((para_coef["delta"])^2 - 4 * para_coef["I(delta^2)"] * para_coef["(Intercept)"])) / (2 * para_coef["I(delta^2)"]) %>%
                unname()
            } else {
              out <- (-para_coef["delta"] - sqrt((para_coef["delta"])^2 - 4 * para_coef["I(delta^2)"] * para_coef["(Intercept)"])) / (2 * para_coef["I(delta^2)"]) %>%
                unname()
            }
          } else {
            if (near(para_coef["I(delta^2)"], 0)) {
              warning("Coefficient for quadratic term is 0. Use 0 shifting.")

              out <- 0
            }
          }

          ### Minimum value of parabola is far from delta with minmimum MSE
          ### Bad fit
          if (!between(out, min(delta), max(delta))) {
            warning("Minimum value of the parabola is out of preset delta range. Use 0 shifting.")

            out <- 0

            # out <- pmin(out, max(delta))
            # out <- pmax(out, min(delta))
          }
        }
        out
      }
    })

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
    attr(ggplot_attrs, "x3p_before_shift_plot") <- x3p_shift_delta_df %>%
      ggplot(aes(x = x, y = y, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      theme_bw()
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
    attr(ggplot_attrs, "x3p_after_shift_plot") <- x3p_approx_df %>%
      ggplot(aes(x = x, y = y, fill = value_approx)) +
      geom_tile() +
      scale_fill_gradient2(midpoint = 0) +
      theme_bw()
  }

  if (ifplot) {
    j <- floor(length(yidx) / 2 - 30)

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
    if (length(MSE) - sum(is.na(MSE)) < 3) {
      warning("No enough non-NA MSE values to fit parabola.")

      return(NA)
    } else {
      para_coef <- lm(MSE ~ delta + I(delta^2)) %>%
        coef()

      ### Get delta with minimum mean squared error
      out <- (-para_coef["delta"] / (2 * para_coef["I(delta^2)"])) %>%
        unname()

      ### Consider different a values
      if (para_coef["I(delta^2)"] < 0) {
        if (out >= 0) {
          out <- (-para_coef["delta"] + sqrt((para_coef["delta"])^2 - 4 * para_coef["I(delta^2)"] * para_coef["(Intercept)"])) / (2 * para_coef["I(delta^2)"]) %>%
            unname()
        } else {
          out <- (-para_coef["delta"] - sqrt((para_coef["delta"])^2 - 4 * para_coef["I(delta^2)"] * para_coef["(Intercept)"])) / (2 * para_coef["I(delta^2)"]) %>%
            unname()
        }
      } else {
        if (near(para_coef["I(delta^2)"], 0)) {
          warning("Coefficient for quadratic term is 0. Use 0 shifting.")

          out <- 0
        }
      }

      ### Minimum value of parabola is far from delta with minmimum MSE
      ### Bad fit
      if (!between(out, min(delta), max(delta))) {
        warning("Minimum value of the parabola is out of preset delta range. Use 0 shifting.")

        out <- 0

        # out <- pmin(out, max(delta))
        # out <- pmax(out, min(delta))
      }
    }
    out

    attr(ggplot_attrs, "MSE_plot") <- tibble(delta = delta, MSE = MSE) %>%
      ggplot(aes(x = delta, y = MSE)) +
      geom_point() +
      geom_smooth(method = "lm", formula = y ~ x + I(x^2)) +
      geom_vline(xintercept = (-para_coef[2] / (2 * para_coef[3])), col = "red") +
      theme_bw() +
      scale_x_continuous(breaks = delta)
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

  attributes(x3p_approx) <- c(attributes(x3p_approx), attributes(ggplot_attrs))

  return(x3p_approx)
}
