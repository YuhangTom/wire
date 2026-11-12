# Minimize MSE by shifting striations

This function shifts the striations on an `x3p` object to minimize the
Mean Squared Error (MSE).

## Usage

``` r
x3p_shift(x3p, ifplot = FALSE, delta = -5:5, delta_q_range = c(0, 1))
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- ifplot:

  A Boolean flag indicating whether to save ggplot lists in the output
  attributes.

- delta:

  A numeric vector representing the shifting range for minimizing MSE.

- delta_q_range:

  A numeric vector of length 2, representing the lower and upper bounds
  for the quantile taken.

## Value

An `x3p` object after the transformation.

## Examples

``` r
x3p <- x3p_subsamples[[2]]

insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
  ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
)
x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = FALSE)

if (interactive()) {
  x3p_approx <- x3p_shift(x3p_bin_rotate, ifplot = TRUE)

  attr(x3p_approx, "x3p_before_shift_plot")
  attr(x3p_approx, "x3p_after_shift_plot")
  attr(x3p_approx, "fn_align_plot")
  attr(x3p_approx, "MSE_plot")
}
```
