context("Sk")

test_that("special cases are correct", {
  print("special cases are correct")
  expect_equal(sk(c(-1/2, 1/2), c(-1/2, 1/2), c(-1, 1), c(-1, 0,  1)), c(exp(0)-exp(-1), (exp(1)-exp(0))))
})

test_that("throw errors for inputs error", {
  print("throw errors when vector length is not consistent")
  expect_error(sk(c(-1/2, 1/2, 1), c(-1/2, 1/2), c(-1, 1), c(-1, 0,  1)), "length inconsistent")
  expect_error(sk(c(-1/2, 1/2, 1), c(-1/2, 1/2), c(-1, 1), c(-1, 0)), "length inconsistent")
  print("throw errors when number of points is less than 2")
  expect_error(sk(c(1), c(1/2), c(1), c(-1, 0)), "number of points should be greater than 2")  
})