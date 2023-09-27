test_that("input check works", {
  expect_error(
    x3p_quantile_angle_vec(1:3,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec("abc",
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(TRUE,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(data.frame(1:3, 2:4),
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = -1, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 0, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 1.1, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = "abc", min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = TRUE, min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = data.frame(1:3, 2:4), min_score_cut = 0.1,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = "abc",
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = TRUE,
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = data.frame(1:3, 2:4),
      ifplot = FALSE
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = 1:3
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = "abc"
    )
  )
  expect_error(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = data.frame(1:3, 2:4)
    )
  )
})


test_that("output plot works", {
  expect_visible(
    x3p_quantile_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = TRUE
    )
  )
})


test_that("output return works", {
  expect_type(
    quantile_angle, "double"
  )
})
