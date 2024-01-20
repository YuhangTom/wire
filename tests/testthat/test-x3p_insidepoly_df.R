test_that("input check works", {
  expect_error(
    x3p_insidepoly_df(1:3, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df("abc", mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(TRUE, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(data.frame(1:3, 2:4), mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = 1, concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = TRUE, concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = data.frame(1:3, 2:4), concavity = 1.5, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = -1, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 0, b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = "abc", b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = data.frame(1:3, 2:4), b = 1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 0, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = -1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1.1, ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = "abc", ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = data.frame(1:3, 2:4), ifplot = FALSE)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = 1)
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = "abc")
  )
  expect_error(
    x3p_insidepoly_df(x3p, mask_col = "#FF0000", concavity = 1.5, b = 1, ifplot = data.frame(1:3, 2:4))
  )
})


test_that("output plot works", {
  expect_visible(
    attr(insidepoly_df, "x3p_plot")
  )

  expect_visible(
    attr(insidepoly_df, "number_of_missing_immediate_neighbors_plot")
  )

  expect_visible(
    attr(insidepoly_df, "standard_deviation_of_non_missing_immediate_neighbors_plot")
  )

  expect_visible(
    attr(insidepoly_df, "number_of_missing_immediate_neighbors_boxplot")
  )
})


test_that("output return works", {
  expect_type(
    insidepoly_df, "list"
  )
  expect_true(
    between(length(insidepoly_df), 5, 6)
  )
  expect_contains(
    names(insidepoly_df), c("x", "y", "value", "mask", "n_neighbor_val_miss")
  )
})
