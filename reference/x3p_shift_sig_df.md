# Extract signal from transformed `x3p` object by minimizing MSE

This function extracts the signal from a transformed `x3p` object by
minimizing the Mean Squared Error (MSE). It computes summary statistics
of values along the `y` axis for each `x` value.

## Usage

``` r
x3p_shift_sig_df(x3p, ifplot = FALSE, delta = -5:5)
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- ifplot:

  A Boolean flag indicating whether to save ggplot lists in the output
  attributes.

- delta:

  A numeric vector representing the shifting range for minimizing MSE.

## Value

A data frame with two columns:

- x: The `x` values from the `x3p` object.

- sig: The extracted signal.

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
  shift_sig_df <- x3p_shift_sig_df(x3p_bin_rotate, ifplot = TRUE)

  attr(shift_sig_df, "x3p_before_shift_plot")
  attr(shift_sig_df, "x3p_after_shift_plot")
  attr(shift_sig_df, "sig_df_plot")
}
```
