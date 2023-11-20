#' Shift striations by minimizing MSE
#'
#' Shift striations on `x3p` object by minimizing MSE.
#' @param x3p `x3p` object
#' @param ifplot whether graphs are displayed
#' @param delta shifting range when minimizing MSE
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
#'   x3p_shift(x3p_bin_rotate, ifplot = TRUE)
#' }
#'
x3p_shift <- function(x3p, ifplot = FALSE, delta = -5:5) {
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
  yidx <- rev(which(colSums(!is.na(x3p$surface.matrix)) >= 2))
  y_sort <- x3p_df$y %>%
    unique() %>%
    sort()
  y_sort <- y_sort[rev(yidx)]

  delta_min <- (1:(length(yidx) - 1)) %>%
    map_dbl(function(j) {
      ### f1 values
      f1 <- x3p$surface.matrix[, yidx[j]]
      f2 <- x3p$surface.matrix[, yidx[j + 1]]

      ### Mean squared error for all delta
      MSE <- map_dbl(delta, function(delta_i) {
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
        (-para_coef[2] / (2 * para_coef[3])) %>%
          unname()
      }
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

  # shift scan horizontally to let it start in 0
  x3p_shift_delta_df <- x3p_shift_delta_df %>%
    filter(!is.na(value)) %>%
    mutate(
      x_shift_delta = x_shift_delta - min(x_shift_delta)
    )

  # switch from scale resolution to (approximate) integer resolution
  x3p_shift_delta_df <- x3p_shift_delta_df %>%
    mutate(
      x_shift_delta = x_shift_delta / scale
    )

  approx_range <- seq(0, ceiling(max(x3p_shift_delta_df$x_shift_delta)))

  # approximate on integer resolution
  x3p_approx_df <- x3p_shift_delta_df %>%
    group_by(y) %>%
    nest(.key = "Dat") %>%
    mutate(Dat = Dat %>% map(.f = function(dat) {
      #  browser()
      if (sum(!is.na(dat$value)) < 2) {
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
