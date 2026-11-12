# Calculate rotation angle using maximum likelihood estimation (MLE)

This function calculates the rotation angle of an `x3p` object using
maximum likelihood estimation (MLE) with Hough transformation.

## Usage

``` r
x3p_MLE_angle_vec(
  x3p,
  ntheta = 720,
  min_score_cut = 0.1,
  ifplot = FALSE,
  loess_span = 0.2
)
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- ntheta:

  The number of bins along the theta axis used in
  [`imager::hough_line`](https://rdrr.io/pkg/imager/man/hough_line.html).

- min_score_cut:

  A tuning parameter that sets the minimum score required for the Hough
  transformation.

- ifplot:

  A Boolean flag indicating whether to save ggplot lists in the output
  attributes.

- loess_span:

  A parameter controlling the degree of smoothing in the LOESS function.

## Value

A vector of rotation angles computed by the MLE procedure.

## Examples

``` r
x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res,
  ifout = FALSE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE
)

x3p_bin <- x3p_inner_impute %>%
  x3ptools::x3p_bin_stripes(
    direction = "vertical",
    colors = c("#b12819", "#ffffff", "#134D6B"),
    freqs = c(0, 0.3, 0.7, 1)
  )
x3p_bin_red <- x3ptools::x3p_extract(x3p_bin, mask_vals = "#b12819")

angle_red <- x3p_MLE_angle_vec(x3p_bin_red, min_score_cut = 5, ifplot = TRUE)

attr(angle_red, "nfline_plot")

attr(angle_red, "MLE_loess_plot")
#> `geom_smooth()` using formula = 'y ~ x'

```
