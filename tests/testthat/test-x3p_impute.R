test_that("input check works", {
  expect_error(
    x3p_impute(1:3, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute("abc", ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(TRUE, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(data.frame(1:3, 2:4), ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = 1, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = "abc", ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = data.frame(1:3, 2:4), ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = 1, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = "abc", dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = data.frame(1:3, 2:4), dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = TRUE, dir_name = 1:3, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = TRUE, dir_name = data.frame(1:3, 2:4), ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = 1)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = "abc")
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifout = TRUE, ifsave = FALSE, dir_name = NULL, ifplot = data.frame(1:3, 2:4))
  )
})

test_that("output plot works", {
  expect_visible(
    attr(x3p_inner_impute, "x3p_impute_0_plot")
  )
  expect_visible(
    attr(x3p_inner_impute, "x3p_impute_1_plot")
  )
  expect_visible(
    attr(x3p_inner_impute, "x3p_impute_n_plot")
  )
})

# test_that("output save works", {
#   dir_name <- "temptest"
#   x3p_impute(x3p_inner_nomiss_res, ifsave = TRUE, dir_name = dir_name, ifplot = TRUE)
#   expect_true(
#     file.exists(
#       paste0(dir_name, "/focal_impute.gif")
#     )
#   )
#   unlink(dir_name, recursive = T, force = T)
# })

test_that("output return works", {
  expect_type(
    x3p_inner_impute, "list"
  )
  expect_length(
    x3p_inner_impute, 3
  )
  expect_named(
    x3p_inner_impute, c("surface.matrix", "header.info", "matrix.info")
  )
  expect_equal(
    x3p_inner_nomiss_res$header.info, x3p_inner_impute$header.info
  )
  expect_equal(
    x3p_inner_nomiss_res$matrix.info, x3p_inner_impute$matrix.info
  )
})
