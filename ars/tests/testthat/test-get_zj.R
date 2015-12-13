context("Intersection points zj")

test_that("special cases are correct", {
    print("special cases are correct")
    expect_equal(get_zj(c(1,2,3), c(4,8,12), c(1, 0,-1)), c(5,7))
})


test_that("throw errors for inputs error", {
    print("throw errors when the inputs have different length")
    expect_error(get_zj(c(1,2,3), c(4,8), c(1, 0, 1)), "inputs should have the same length")
    expect_error(get_zj(c(1,2,3), c(4,8,12), c(1, 0)), "inputs should have the same length")
    expect_error(get_zj(c(1,2,3), c(4,8,12,14), c(1,0,1)), "inputs should have the same length")
    print("throw errors when the number of points is less than 2")
    expect_error(get_zj(1,4,1), "input should have a length at least 2")
})