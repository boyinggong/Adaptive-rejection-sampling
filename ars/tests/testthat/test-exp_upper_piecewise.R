context("Exponential of the upper piecewise")

test_that("special cases are correct", {
    print("special cases are correct")
    temp <- upper_piecewise(2, c(1,2,3,4), c(3,5,6,9),c(1,2,-3,0),c(-3,3))
    expect_equal(exp(temp), exp_upper_piecewise(2, c(1,2,3,4), c(3,5,6,9),c(1,2,-3,0),c(-3,3)))
})

test_that("throw errors for inputs error", {
    print("throw errors when vector length is not consistent")
    expect_error(exp_upper_piecewise(2, c(1,2,3), c(3,5,6,9),c(1,2,-3,0),c(-3,3)), "input length inconsistent")
    expect_error(exp_upper_piecewise(2, c(1,2,3,4), c(3,5,6,9),c(1,-3),c(-3,3)), "input length inconsistent")
    print("throw errors when number of points is less than 2")
    expect_error(exp_upper_piecewise(2,1,4,0,c(-3,3), "number of points should be greater than 2")
})