test_that("input check works", {
  expect_error(
    x3p_impute(1:3, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute("abc", ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(TRUE, ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(data.frame(1:3, 2:4), ifsave = FALSE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = 1, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = "abc", dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = data.frame(1:3, 2:4), dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = TRUE, dir_name = NULL, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = TRUE, dir_name = 1:3, ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = TRUE, dir_name = data.frame(1:3, 2:4), ifplot = FALSE)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = 1)
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = "abc")
  )
  expect_error(
    x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = data.frame(1:3, 2:4))
  )
})

test_that("output plot works", {
  expect_visible(
    x3p_impute(x3p_inner_nomiss_res, ifsave = FALSE, dir_name = NULL, ifplot = TRUE)
  )
})

# test_that("output save works", {
#   withr::with_tempdir({
#     dir_name <- "."
#     x3p_impute(x3p_inner_nomiss_res, ifsave = TRUE, dir_name = dir_name, ifplot = TRUE)
#     expect_true(
#       file.exists(
#         paste0(dir_name, "/focal_impute.gif")
#       )
#     )
#   })
# })

test_that("output return works", {
  expect_type(
    x3p_inner_impute, "list"
  )
  expect_length(
    x3p_inner_impute, 4
  )
  expect_named(
    x3p_inner_impute, c("surface.matrix", "header.info", "matrix.info", "mask")
  )
  expect_equal(
    x3p_inner_nomiss_res$header.info, x3p_inner_impute$header.info
  )
  expect_equal(
    x3p_inner_nomiss_res$matrix.info, x3p_inner_impute$matrix.info
  )
})
