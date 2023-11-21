
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wire

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Last-changedate](https://img.shields.io/badge/last%20change-2023--11--20-yellowgreen.svg)](https://github.com/YuhangTom/wire/commits/main)
[![Codecov test
coverage](https://codecov.io/gh/YuhangTom/wire/branch/main/graph/badge.svg)](https://app.codecov.io/gh/YuhangTom/wire?branch=main)
[![R-CMD-check](https://github.com/YuhangTom/wire/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/YuhangTom/wire/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `wire` is to implement a systematic reproducible automatic
algorithm to analyze similarity between wire cut scans.

# Installation

You can install the development version of wire from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("YuhangTom/wire")
```

# Usage

## Data

The original scans for the wire cuts are stored in `x3p` format of width
around $2,300$ and height around $1,800$, under a resolution
$0.645 \mu m \times 0.645 \mu m$, with each file being at least 15 MB,
which is much larger compared to the file limit of 5 MB for a `R`
package. Therefore, we make available 2 subsampled `x3p` data set by
every 10 observations, saved as entries with its label in a `list`
object, named as `x3p_subsamples`. The data can be used by:

``` r
library(wire)

x3p_subsamples
#> $`T2AW-LM-R2-B32`
#> x3p object
#> size (width x height): 231 x 182 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00 
#> 
#> $`T2CW-LI-R2-B15`
#> x3p object
#> size (width x height): 231 x 179 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00
```

## Inner polygon

To remove edge effect, we extract the inner part of the scan, which can
be achieved by:

``` r
x3p <- x3p_subsamples[[1]]
insidepoly_df <- x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = TRUE)
```

<img src="man/figures/README-insidepoly-1.png" width="100%" /><img src="man/figures/README-insidepoly-2.png" width="100%" /><img src="man/figures/README-insidepoly-3.png" width="100%" />

    #> Warning: Removed 19502 rows containing non-finite values (`stat_boxplot()`).

<img src="man/figures/README-insidepoly-4.png" width="100%" />

``` r
insidepoly_df %>%
  str()
#> 'data.frame':    42042 obs. of  6 variables:
#>  $ x                  : num  0 6.45 12.9 19.35 25.8 ...
#>  $ y                  : num  1167 1167 1167 1167 1167 ...
#>  $ value              : num  NA NA NA NA NA NA NA NA NA NA ...
#>  $ mask               : chr  "#FFFFFF" "#FFFFFF" "#FFFFFF" "#FFFFFF" ...
#>  $ n_neighbor_val_miss: Factor w/ 11 levels "0","1","2","3",..: 11 11 11 11 11 11 11 11 11 11 ...
#>  $ sd_not_miss        : num  NaN NaN NaN NaN NaN NaN NaN NaN NaN NaN ...
#>  - attr(*, "header.info")=List of 4
#>   ..$ sizeX     : int 231
#>   ..$ sizeY     : int 182
#>   ..$ incrementX: num 6.45
#>   ..$ incrementY: num 6.45
```

## Remove trend

To remove overall trend on the inner surface, we can use:

``` r
x3p_inner_nomiss_res <- df_rmtrend_x3p(insidepoly_df)
x3p_inner_nomiss_res
#> x3p object
#> size (width x height): 231 x 182 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00
```

## Imputation

We can impute the missing values inside the surface by:

``` r
x3p_inner_impute <- x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = TRUE)
```

<img src="man/figures/README-impute-1.png" width="100%" /><img src="man/figures/README-impute-2.png" width="100%" /><img src="man/figures/README-impute-3.png" width="100%" />

``` r
x3p_inner_impute
#> x3p object
#> size (width x height): 231 x 182 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00
```

## Rotation

We can rotate the surface to the correct angle by:

``` r
x3p_bin_rotate <- x3p_vertical(x3p_inner_impute, min_score_cut = 0.1, ifplot = TRUE)
```

<img src="man/figures/README-rotate-1.png" width="100%" /><img src="man/figures/README-rotate-2.png" width="100%" /><img src="man/figures/README-rotate-3.png" width="100%" /><img src="man/figures/README-rotate-4.png" width="100%" /><img src="man/figures/README-rotate-5.png" width="100%" /><img src="man/figures/README-rotate-6.png" width="100%" /><img src="man/figures/README-rotate-7.png" width="100%" /><img src="man/figures/README-rotate-8.png" width="100%" /><img src="man/figures/README-rotate-9.png" width="100%" /><img src="man/figures/README-rotate-10.png" width="100%" /><img src="man/figures/README-rotate-11.png" width="100%" /><img src="man/figures/README-rotate-12.png" width="100%" /><img src="man/figures/README-rotate-13.png" width="100%" /><img src="man/figures/README-rotate-14.png" width="100%" />

``` r
x3p_bin_rotate
#> x3p object
#> size (width x height): 200 x 149 in pixel 
#> resolution: 6.4500e+00 x 6.4500e+00
```

## Signal extraction

To extract signals from the rotated surface, two methods are provided,
implemented by `wire::x3p_raw_sig_vec` and `wire::x3p_shift_sig_vec`,
respectively:

``` r
raw_sig <- x3p_raw_sig_vec(x3p_bin_rotate, ifplot = TRUE)
#> Warning: Removed 7215 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-signal-1.png" width="100%" />

``` r
raw_sig %>%
  str()
#> tibble [200 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ x  : num [1:200] 0 6.45 12.9 19.35 25.8 ...
#>  $ sig: num [1:200] 1.852 1.916 1.759 1.572 0.794 ...

shift_sig <- x3p_shift_sig_vec(x3p_bin_rotate, ifplot = TRUE)
#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.
#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.

#> Warning in .f(.x[[i]], ...): Minimum value of the parabola is out of preset
#> delta range. Use 0 shifting.
#> Warning: Removed 8 rows containing missing values (`geom_line()`).
```

<img src="man/figures/README-signal-2.png" width="100%" /><img src="man/figures/README-signal-3.png" width="100%" /><img src="man/figures/README-signal-4.png" width="100%" />

    #> Warning in snapshot3d(scene = x, width = width, height = height): webshot =
    #> TRUE requires the webshot2 package and Chrome browser; using rgl.snapshot()
    #> instead
    #> Warning in snapshot3d(scene = x, width = width, height = height): webshot =
    #> TRUE requires the webshot2 package and Chrome browser; using rgl.snapshot()
    #> instead

<img src="man/figures/README-signal-5.png" width="100%" />

    #> Warning: Removed 6081 rows containing missing values (`geom_line()`).
    #> Warning: Removed 1 row containing missing values (`geom_line()`).

<img src="man/figures/README-signal-6.png" width="100%" />

``` r
shift_sig %>%
  str()
#> tibble [201 × 2] (S3: tbl_df/tbl/data.frame)
#>  $ x  : num [1:201] 0 6.45 12.9 19.35 25.8 ...
#>  $ sig: num [1:201] 2.38 2.38 1.91 1.7 1.3 ...
```

## Signal alignment

Extracted signals can be aligned and cross correlation can be computed:

``` r
vec_align_sigs_list(raw_sig$sig, shift_sig$sig, ifplot = TRUE) %>%
  str()
#> Warning: Removed 1 row containing missing values (`geom_line()`).
#> Removed 1 row containing missing values (`geom_line()`).
```

<img src="man/figures/README-align-1.png" width="100%" />

    #> List of 3
    #>  $ ccf  : num 0.982
    #>  $ lag  : num 1
    #>  $ lands:'data.frame':   201 obs. of  3 variables:
    #>   ..$ x   : int [1:201] 1 2 3 4 5 6 7 8 9 10 ...
    #>   ..$ sig1: num [1:201] NA 1.85 1.92 1.76 1.57 ...
    #>   ..$ sig2: num [1:201] 2.38 2.38 1.91 1.7 1.3 ...
