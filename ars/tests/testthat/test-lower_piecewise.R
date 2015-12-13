context("Lower piecewise function")

#test_that("special cases are correct", {
#    print("special cases are correct")
#    expect_equal(lower_piecewise(2,c(1,2,3,4), c(5,6,7,8), c(-4,4)),6)
#})

test_that("throw errors for inputs error", {
    print("throw errors when vector length is not consistent")
    expect_error((lower_piecewise(2,c(1,2), c(5,6,7,8), c(-4,4)), "input length inconsistent")
    print("throw errors when number of points is less than 2")
    expect_error((lower_piecewise(2,1, 5, c(-4,4)), "number of points should be greater than 2")
})