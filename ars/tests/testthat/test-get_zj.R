context("Intersection points")

test_that("special cases are correct", {
    print("special cases are correct")
    expect_equal(get_zj(c(1,2,3), c(4,8,12), c(1, 0,-1)), c(5,7))
})

test_that("throw errors for inputs error", {
    print("throw errors when vector length is not consistent")
    expect_error(get_zj(c(1,2,3), c(4,8), c(1, 0, 1), c(-1, 0,  1)), "input length inconsistent")
    expect_error(get_zj(c(1,2,3), c(4,8,12), c(1, 0)), "input length inconsistent")
    print("throw errors when number of points is less than 2")
    expect_error(get_zj(c(1), c(4), c(1)), "number of points should be greater than 2")
})