test_that("input check works", {
  expect_error(
    x3p_shift(1:3, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift("abc", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift(TRUE, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift(data.frame(1:3, 2:4), ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = 1, delta = -5:5)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = "abc", delta = -5:5)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = data.frame(1:3, 2:4), delta = -5:5)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = FALSE, delta = 0:1)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = FALSE, delta = "abc")
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = FALSE, delta = TRUE)
  )
  expect_error(
    x3p_shift(x3p_bin_rotate, ifplot = FALSE, delta = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    attr(x3p_approx, "x3p_before_shift_plot")
  )
  expect_visible(
    attr(x3p_approx, "x3p_after_shift_plot")
  )
  expect_visible(
    attr(x3p_approx, "MSE_plot")
  )
  expect_visible(
    attr(x3p_approx, "fn_align_plot")
  )
})


test_that("output return works", {
  expect_type(
    x3p_approx, "list"
  )
  expect_length(
    x3p_approx, 5
  )
  expect_named(
    x3p_approx, c("groups", "surface.matrix", "header.info", "matrix.info", "mask")
  )
})
