# Smooth the signal and remove the hooks

This function smooths the signal and removes the hooks by running a
t-test in the boundaries to see if there are large 'hooks'.

## Usage

``` r
df_ccsig(
  sig_df,
  span1 = 400,
  span2 = 40,
  breaks_q = c(0, 0.025, 0.05, 0.95, 0.975, 1),
  outlier_cycles = 1,
  ifplot = FALSE
)
```

## Arguments

- sig_df:

  A signal data frame with two columns: `x` and `sig`.

- span1:

  A positive parameter to control the degree of smoothing in
  bullterxtrctr::cc_get_signature.

- span2:

  A positive parameter to control the degree of smoothing in
  bullterxtrctr::cc_get_signature.

- breaks_q:

  A vector of quantiles to split the `x` values into five groups for
  t-test of large hooks.

- outlier_cycles:

  a positive integer for the number of times we check for outliers along
  the boundaries.

- ifplot:

  A Boolean flag indicating whether to save ggplot lists in the output
  attributes.

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
raw_sig_df <- x3p_raw_sig_df(x3p_bin_rotate, ifplot = FALSE)

raw_ccsig_df <- df_ccsig(raw_sig_df, ifplot = TRUE)
#> Warning: k-d tree limited by memory. ncmax= 208

attr(raw_ccsig_df, "sig_df_plot")
#> Warning: Removed 6 rows containing missing values or values outside the scale range
#> (`geom_line()`).

```
