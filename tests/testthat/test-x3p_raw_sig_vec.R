test_that("input check works", {
  expect_error(
    x3p_raw_sig_vec(1:3, method = "median", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec("abc", method = "median", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(TRUE, method = "median", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(data.frame(1:3, 2:4), method = "median", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = "abc", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = TRUE, ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = data.frame(1:3, 2:4), ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = "median", ifplot = 1)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = "median", ifplot = "abc")
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, method = "median", ifplot = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    x3p_raw_sig_vec(x3p_bin_rotate, method = "median", ifplot = TRUE)
  )
})


test_that("output return works", {
  expect_type(
    raw_sig, "double"
  )
  expect_equal(
    x3p_bin_rotate$matrix.info$MatrixDimension$SizeX[[1]], length(raw_sig)
  )
})
