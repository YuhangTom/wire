# Removing missing values and a quadratic trend

This function takes an `x3p` object, specifically the inner polygon data
frame, and performs two main operations:

1.  Removes missing values.

2.  Detrends the data by removing a quadratic trend.

## Usage

``` r
df_rmtrend_x3p(insidepoly_df)
```

## Arguments

- insidepoly_df:

  A data frame representing the inner polygon. This is typically
  obtained from the
  [`wire::x3p_insidepoly_df`](https://yuhangtom.github.io/wire/reference/x3p_insidepoly_df.md)
  function.

## Value

An `x3p` object that contains the residuals after the removal of the
quadratic trend.

## Examples

``` r
x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1)

x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_nomiss_res
#> x3p object
#> size (width x height): 239 x 172 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00 
if (interactive()) {
  x3p_image_autosize(x3p_inner_nomiss_res)
}
```
