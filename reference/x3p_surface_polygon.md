# Add a polygon mask of the scan shape

This function adds a polygon mask to an `x3p` scan. The polygon
represents the shape of the scan.

## Usage

``` r
x3p_surface_polygon(
  x3p,
  colour = "red",
  sample = 10,
  center = NULL,
  concavity = 1.5
)
```

## Arguments

- x3p:

  An `x3p` object representing a topographic scan.

- colour:

  A string specifying the colour of the polygon mask.

- sample:

  A positive integer indicating the downsampling rate used to calculate
  the boundary points of the polygon. Higher values result in faster
  computation but less accurate polygons.

- center:

  A numeric vector representing the center point of the scan. If `NULL`,
  the center point is derived from the boundary points.

- concavity:

  A positive number used in the
  [`concaveman::concaveman`](https://joelgombin.github.io/concaveman/reference/concaveman.html)
  function to determine the concavity of the polygon.

## Value

An `x3p` object with an added polygon mask.

## Examples

``` r
if (interactive()) {
  x3p <- x3p_subsamples[[1]]
  x3p <- x3p %>% x3p_surface_polygon(sample=1)
  x3p_image(x3p, size = dim(x3p$surface.matrix), zoom=.6)
}
```
