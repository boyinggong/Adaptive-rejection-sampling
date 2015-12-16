context("Derivative of logarithm of functions")

test_that("special cases is correct", {
  print("special cases is correct")
  expect_equal(dh(c(1,2,3),dexp), c(-1,-1,-1))
  expect_equal(dh(c(1,2,3),dnorm), c(-1,-2,-3))
})