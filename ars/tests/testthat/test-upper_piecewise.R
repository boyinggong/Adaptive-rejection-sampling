context("Upper piecewise function")

test_that("throw errors for inputs error", {
  print("throw errors when input x are not within the domain")
  expect_error(upper_piecewise(2,c(1,2), c(5,7), c(1,2), c(3,4,5)), 
               "inputs are not defined on the given domain")
  expect_error(upper_piecewise(2,c(1,2), c(5,7), c(1,2), c(-1,0,1)), 
               "inputs are not defined on the given domain")
  
  print("throw errors when inputs length inconsistent")
  expect_error(upper_piecewise(2,c(1,2), c(5,6,7,8), c(1,2,3), c(-4,4,5,6)), 
               "inputs length inconsistent")
  expect_error(upper_piecewise(2,c(1,2), c(5,6,7), c(1,2,3), c(-4,4,5)), 
               "inputs length inconsistent")
  expect_error(upper_piecewise(2,c(1,2), c(5,6,7), c(1,2,3, 4), c(-4,4,5,6)), 
               "inputs length inconsistent")
})