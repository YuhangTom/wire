test_that("input check works", {
  expect_error(
    x3p_image_autosize(1:3, ifhtml = FALSE, zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize("abc", ifhtml = FALSE, zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(TRUE, ifhtml = FALSE, zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(data.frame(1:3, 2:4), ifhtml = FALSE, zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = 1:3, zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = "abc", zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = data.frame(1:3, 2:4), zoom = 0.6)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = FALSE, zoom = -1)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = FALSE, zoom = 0)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = FALSE, zoom = "abc")
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = FALSE, zoom = TRUE)
  )
  expect_error(
    x3p_image_autosize(x3p, ifhtml = FALSE, zoom = data.frame(1:3, 2:4))
  )
})

