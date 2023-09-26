test_that("input check works", {
  expect_error(
    inside_polygon(x = "abc", y = 2:4, concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = TRUE, y = 2:4, concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = data.frame(1:3, 2:4), y = 2:4, concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = "abc", concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = TRUE, concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = data.frame(1:3, 2:4), concavity = 1.5, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = -1, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = 0, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = "abc", center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = TRUE, center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = data.frame(1:3, 2:4), center = NULL)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = "abc")
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = TRUE)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = 1:3)
  )
  expect_error(
    inside_polygon(x = 1:3, y = 2:4, concavity = 1.5, center = data.frame(1:3, 2:4))
  )
})


test_that("output return works", {
  expect_type(
    insidepoly, "list"
  )
  expect_length(
    insidepoly, 7
  )
  expect_named(
    insidepoly,
    c("xout", "yout", "rout", "theta", "x", "y", "id")
  )
})
