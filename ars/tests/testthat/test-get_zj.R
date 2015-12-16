context("Intersection points")

test_that("special cases are correct", {
  expect_equal(get_zj(c(1/3, 2/3), c(0, 0), c(0, 0)), 1/2)
})