#' Extract signal with transformed `x3p` object by minimizing MSE
#'
#' Extract signal by computing summary statistics of values along `y` for each `x` with transformed `x3p` object by minimizing MSE.
#' @param x3p `x3p` object
#' @param method choice of `median` or `mean` when computing the summary statistics
#' @param ifplot whether graphs are displayed
#' @param delta shifting range when minimizing MSE
#' @return data frame of 2 columns
#' * x: x value
#' * sig: signal extracted
#' @import dplyr
#' @importFrom x3ptools x3p_to_df x3p_delete_mask x3p_bin_stripes
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' @importFrom stats na.omit median lm coef approx
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
#' ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
#' x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)
#'
#' if (interactive()) {
#'   x3p_shift_sig_vec(x3p_bin_rotate, ifplot = TRUE) %>%
#'   str()
#' }
#'
x3p_shift_sig_vec <- function(x3p, method = "median", ifplot = FALSE, delta = -5:5) {
  assert_that(
    "x3p" %in% class(x3p),
    method %in% c("median", "mean"),
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
    mask <-
    sig <-
    NULL

  x3p_df <- x3p %>%
    x3p_to_df()

  ### Number of non-missing values for each y
  x3p_df_nobs <- x3p_df %>%
    na.omit() %>%
    group_by(y) %>%
    summarise(value_nobs = n()) %>%
    arrange(value_nobs)

  ### Sort unique y values
  y_sort <- inner_join(x3p_df, x3p_df_nobs, by = join_by(y)) %>%
    ### Filter to have at least 2 observations for approx later
    filter(value_nobs >= 2) %>%
    distinct(y) %>%
    arrange(y) %>%
    unlist()

  ### delta values for minimum mean squared error
  delta_min <- (1:(length(y_sort) - 1)) %>%
    map_dbl(function(j) {
      ### f1 values
      f1 <- x3p_df %>%
        filter(y == y_sort[j]) %>%
        select(x, value) %>%
        arrange(x)

      ### f2 values
      f2 <- x3p_df %>%
        filter(y == y_sort[j + 1]) %>%
        select(x, value) %>%
        arrange(x)

      ### Mean squared error for all delta
      MSE <- map_dbl(delta, function(delta_i) {
        if (delta_i == 0) {
          mean((f1$value - f2$value)^2, na.rm = TRUE)
        } else {
          if (delta_i > 0) {
            mean((f1[-(1:delta_i), ]$value - f2[1:(nrow(f2) - delta_i), ]$value)^2, na.rm = TRUE)
          } else {
            mean((f1[1:(nrow(f1) - abs(delta_i)), ]$value - f2[-(1:abs(delta_i)), ]$value)^2, na.rm = TRUE)
          }
        }
      }) %>%
        set_names(delta)

      ### Fit parabola
      para_coef <- lm(MSE ~ delta + I(delta^2)) %>%
        coef()

      ### Get delta with minimum mean squared error
      (-para_coef[2] / (2 * para_coef[3])) %>%
        unname()
    })

  if (ifplot) {
    p_delta <- data.frame(
      y_sort = y_sort,
      delta_min = c(delta_min, NA)
    ) %>%
      ggplot(aes(x = y_sort, y = delta_min)) +
      geom_line()
    print(p_delta)
  }

  ### Cumulative delta
  x_cumdelta <- c(
    ### For the largest y
    0,
    cumsum(ifelse(is.na(delta_min), 0, delta_min))
  )

  if (ifplot) {
    p_cumdelta <- data.frame(
      y_sort = y_sort,
      x_cumdelta = x_cumdelta
    ) %>%
      ggplot(aes(x = y_sort, y = x_cumdelta)) +
      geom_line()
    print(p_cumdelta)
  }

  ### Combine with y values
  x_shift_delta_value_df <- data.frame(
    ### Fix x curve with largest y
    y = y_sort,
    ### Shift x by increment in x
    x_shift_delta_value = x_cumdelta * x3p$header.info$incrementX
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

  x3p_approx_df <- x3p_shift_delta_df %>%
    group_by(y) %>%
    nest(.key = "Dat") %>%
    mutate(Dat = Dat %>% map(.f = function(dat) {
      dat$value_approx <- approx(x = dat$x_shift_delta, y = dat$value, xout = dat$x)$y
      dat
    })) %>%
    unnest(Dat) %>%
    select(x, y, value, value_approx, mask)

  if (ifplot) {
    (x3p_approx_df %>%
      ggplot(aes(x = x, y = y, color = value_approx)) +
      geom_point()) %>%
      print()
  }

  if (ifplot) {
    x3p_image_autosize(x3p, ifhtml = TRUE)

    ### Remove mask and add again, as mask cannot be approx
    x3p_approx_df %>%
      df_to_x3p(var = "value_approx") %>%
      x3p_delete_mask() %>%
      x3p_bin_stripes(
        direction = "vertical",
        colors = c("#b12819", "#ffffff", "#134D6B"),
        freqs = c(0, 0.3, 0.7, 1)
      ) %>%
      x3p_image_autosize(ifhtml = TRUE)
  }

  if (ifplot) {
    p_all <- x3p_approx_df %>%
      ggplot(aes(x = x, y = value_approx)) +
      geom_line(aes(group = y), alpha = 0.1)
  }

  shift_sig <- x3p_approx_df %>%
    na.omit() %>%
    group_by(x) %>%
    summarise(sig = ifelse(method == "median", median(value_approx, na.rm = TRUE),
      ifelse(method == "mean", mean(value_approx, na.rm = TRUE),
        stop('Not an applicable method, choose from "method = median" or "method = mean"')
      )
    ))

  if (ifplot) {
    (p_all +
      geom_line(aes(x = x, y = sig), data = shift_sig, color = "red")) %>%
      print()
  }

  return(shift_sig)
}