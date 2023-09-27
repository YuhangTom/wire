test_that("input check works", {
  expect_error(
    x3p_boundary_points(1:3, sample = 3)
  )
  expect_error(
    x3p_boundary_points("abc", sample = 3)
  )
  expect_error(
    x3p_boundary_points(TRUE, sample = 3)
  )
  expect_error(
    x3p_boundary_points(data.frame(1:3, 2:4), sample = 3)
  )
  expect_error(
    x3p_boundary_points(x3p, sample = -1)
  )
  expect_error(
    x3p_boundary_points(x3p, sample = 0)
  )
  expect_error(
    x3p_boundary_points(x3p, sample = 1.1)
  )
  expect_error(
    x3p_boundary_points(x3p, sample = "abc")
  )
  expect_error(
    x3p_boundary_points(x3p, sample = TRUE)
  )
  expect_error(
    x3p_boundary_points(x3p, sample = data.frame(1:3, 2:4))
  )
})


test_that("output return works", {
  expect_type(
    boundary, "list"
  )
  expect_length(
    boundary, 2
  )
  expect_named(
    boundary, c("y", "x")
  )
})

