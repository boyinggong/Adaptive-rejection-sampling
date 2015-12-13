context("Upper piecewise function")

test_that("special cases are correct", {
    print("special cases are correct")
    expect_equal(upper_piecewise(4,c(1,23,3),c(2,1,4),c(-1,0,10),c(-5,5)),-1)
})

test_that("special cases are correct", {
    print("special cases are correct")
    expect_equal(upper_piecewise(2,c(1,2,3,4), c(5,6,7,8), c(1,0,-1,9),c(-4,4)),6)
})


test_that("throw errors for inputs error", {
    print("throw errors when input x are not within the domain")
    expect_error((upper_piecewise(2,c(1,2,3), c(5,7,8), c(1,2,3) c(3,4)), "inputs are not defined on the given domain")
    print("throw errors when input vectors have different lengths")
    expect_error((upper_piecewise(2,c(1,2), c(5,6,7,8), c(1,2,3) c(-4,4)), "inputs should have the same length")
    print("throw errors when number of points is less than 2")
    expect_error((upper_piecewise(2,1,4, 5, c(-4,4)), "number of points should be greater than 2")
})