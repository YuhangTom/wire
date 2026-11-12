# Extract boundary points from a 3D topographic scan

This function identifies and extracts the boundary points from a 3D
topographic scan in `x3p` format.

## Usage

``` r
x3p_boundary_points(x3p, sample)
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- sample:

  A positive integer that defines the sampling rate for the `x` and `y`
  directions. Every `sample` value will be included in the output.
  Higher values lead to faster computation but a less precise boundary
  representation.

## Value

A data frame containing the boundary points. Each row represents a point
on the boundary, with `x` and `y` columns indicating its coordinates.

## Examples

``` r
x3p <- x3p_subsamples[[1]]
bounds <- x3p_boundary_points(x3p, 2)

library(ggplot2)
library(dplyr)
bounds %>%
  ggplot(aes(x = x, y = y)) +
  geom_point() +
  theme_bw()

```
