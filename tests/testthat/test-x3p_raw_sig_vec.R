test_that("input check works", {
  expect_error(
    x3p_raw_sig_vec(1:3, ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec("abc", ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(TRUE, ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(data.frame(1:3, 2:4), ifplot = FALSE)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, ifplot = 1)
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, ifplot = "abc")
  )
  expect_error(
    x3p_raw_sig_vec(x3p_bin_rotate, ifplot = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    x3p_raw_sig_vec(x3p_bin_rotate, ifplot = TRUE)
  )
})


test_that("output return works", {
  expect_type(
    raw_sig, "list"
  )
  expect_equal(
    x3p_bin_rotate$matrix.info$MatrixDimension$SizeX[[1]], nrow(raw_sig)
  )
})
