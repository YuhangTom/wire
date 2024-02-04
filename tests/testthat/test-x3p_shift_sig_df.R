test_that("input check works", {
  expect_error(
    x3p_shift_sig_df(1:3, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df("abc", ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(TRUE, ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(data.frame(1:3, 2:4), ifplot = FALSE, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = 1, delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = "abc", delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = data.frame(1:3, 2:4), delta = -5:5)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = FALSE, delta = 0:1)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = FALSE, delta = "abc")
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = FALSE, delta = TRUE)
  )
  expect_error(
    x3p_shift_sig_df(x3p_bin_rotate, ifplot = FALSE, delta = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    attr(shift_sig_df, "x3p_before_shift_plot")
  )
  expect_visible(
    attr(shift_sig_df, "x3p_after_shift_plot")
  )
  expect_visible(
    attr(shift_sig_df, "MSE_plot")
  )
  expect_visible(
    attr(shift_sig_df, "sig_df_plot")
  )
})


test_that("output return works", {
  expect_type(
    shift_sig_df, "list"
  )
})
