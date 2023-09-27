test_that("input check works", {
  expect_error(
    x3p_surface_polygon(1:3, colour = "red", sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon("abc", colour = "red", sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(TRUE, colour = "red", sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(data.frame(1:3, 2:4), colour = "red", sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = 1:3, sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = TRUE, sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = data.frame(1:3, 2:4), sample = 10, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = -1, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 0, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 1.1, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = "abc", center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = TRUE, center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = data.frame(1:3, 2:4), center = NULL, concavity = 1.5)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = -1)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = 0)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = "abc")
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = TRUE)
  )
  expect_error(
    x3p_surface_polygon(x3p, colour = "red", sample = 10, center = NULL, concavity = data.frame(1:3, 2:4))
  )
})


test_that("output return works", {
  expect_type(
    poly, "list"
  )
  expect_length(
    poly, 5
  )
  expect_named(
    poly, c("class", "surface.matrix", "header.info", "matrix.info", "mask")
  )
})

