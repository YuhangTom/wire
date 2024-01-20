test_that("input check works", {
  expect_error(
    x3p_vertical(1:3,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical("abc",
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(TRUE,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(data.frame(1:3, 2:4),
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.5, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0.1, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 0.9),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = "abc",
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = TRUE,
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = data.frame(1:3, 2:4),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = -1, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 0, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 1.1, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = "abc", min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = TRUE, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = data.frame(1:3, 2:4), min_score_cut = 0.1, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = "abc", ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = TRUE, ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = data.frame(1:3, 2:4), ifplot = FALSE, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = 1, loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = "abc", loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = data.frame(1:3, 2:4), loess_span = 0.2
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = -1
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = 0
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = "abc"
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = TRUE
    )
  )
  expect_error(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = FALSE, loess_span = data.frame(1:3, 2:4)
    )
  )
})

test_that("output plot works", {
  expect_visible(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = TRUE, loess_span = 0.2
    )
  )
  expect_visible(
    x3p_vertical(x3p_inner_impute,
      freqs = c(0, 0.3, 0.7, 1),
      ntheta = 720, min_score_cut = 0.1, ifplot = TRUE, loess_span = 0.2
    )
  )
})

test_that("output return works", {
  expect_type(
    x3p_bin_rotate, "list"
  )
  expect_length(
    x3p_bin_rotate, 4
  )
  expect_named(
    x3p_bin_rotate, c("surface.matrix", "header.info", "matrix.info", "mask")
  )
})
