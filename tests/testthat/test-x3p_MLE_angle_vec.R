test_that("input check works", {
  expect_error(
    x3p_MLE_angle_vec(1:3,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec("abc",
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(TRUE,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(data.frame(1:3, 2:4),
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = -1, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 0, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 1.1, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = "abc", min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = TRUE, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = data.frame(1:3, 2:4), min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = "abc",
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = TRUE,
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = data.frame(1:3, 2:4),
      ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = 1:3, loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = "abc", loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = data.frame(1:3, 2:4), loess_span = 0.2
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = -1
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = 0
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = "abc"
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = TRUE
    )
  )
  expect_error(
    x3p_MLE_angle_vec(x3p_inner_impute,
      ntheta = 720, min_score_cut = 0.1,
      ifplot = FALSE, loess_span = data.frame(1:3, 2:4)
    )
  )
})


test_that("output plot works", {
  expect_visible(
    attr(MLE_angle, "nfline_plot")
  )
  expect_visible(
    attr(MLE_angle, "MLE_loess_plot")
  )
})


test_that("output return works", {
  expect_type(
    MLE_angle, "double"
  )
})
