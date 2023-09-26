test_that("input check works", {
  expect_error(
    x3p_shift_sig_vec(1:3, method = "median", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec("abc", method = "median", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(TRUE, method = "median", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(data.frame(1:3, 2:4), method = "median", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = 1, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "abc", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = TRUE, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = data.frame(1:3, 2:4), ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = 1, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = "abc", delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = data.frame(1:3, 2:4), delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE, delta = 0:1)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE, delta = "abc")
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE, delta = TRUE)
  )
  expect_error(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = FALSE, delta = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    x3p_shift_sig_vec(x3p_bin_rotate, method = "median", ifplot = TRUE, delta = -5:5)
  )
})


test_that("output return works", {
  expect_type(
    shift_sig, "double"
  )
})
