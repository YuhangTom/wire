test_that("input check works", {
  expect_error(
    df_rmtrend_x3p(1)
  )
  expect_error(
    df_rmtrend_x3p("abc")
  )
  expect_error(
    df_rmtrend_x3p(TRUE)
  )
  expect_error(
    df_rmtrend_x3p(x3p)
  )
  expect_error(
    df_rmtrend_x3p(data.frame(1:3, 2:4))
  )
})


test_that("output return works", {
  expect_type(
    x3p_inner_nomiss_res, "list"
  )
  expect_length(
    x3p_inner_nomiss_res, 4
  )
  expect_named(
    x3p_inner_nomiss_res, c("surface.matrix", "header.info", "matrix.info", "mask")
  )
  expect_equal(
    x3p$header.info, x3p_inner_nomiss_res$header.info
  )
  expect_equal(
    x3p$matrix.info, x3p_inner_nomiss_res$matrix.info
  )
})

