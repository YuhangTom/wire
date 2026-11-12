# Display an `x3p` object as an auto-sized image

This function displays an `x3p` object as an image with automatically
adjusted size based on the dimensions of the `x3p` object.

## Usage

``` r
x3p_image_autosize(x3p, ifhtml = FALSE, zoom = 0.6, ...)
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- ifhtml:

  A Boolean flag indicating whether the image will be embedded in HTML.

- zoom:

  A numeric value indicating the zoom level for the image display.

- ...:

  Additional parameters for
  [`x3ptools::x3p_image`](https://heike.github.io/x3ptools/reference/x3p_image.html),
  excluding `size` and `zoom`.

## Value

An `rgl` plot. If `ifhtml = TRUE`, a list of HTML tags is returned.

## Examples

``` r
x3p <- x3p_subsamples[[1]]
if (interactive()) {
  x3p_image_autosize(x3p)
}
```
