context("Derivative of logarithm of functions")

test_that("special cases is correct", {
    print("special cases is correct")
    xi <- c(1,2,3)
    expect_equal(dh(xi,exp,-4,4), c(1,1,1))
})

test_that("special cases is correct", {
    xi <- c(7,8,9)
    expect_equal(dh(xi,log,-3,3), c(1/(7*log(7)),1/(8*log(8)),1/(9*log(9))))
})

test_that("throw errors for inputs error", {
    print("throw errors when input x values are not defined on the given domain")
    expect_error(dh(c(7,8,9),exp,-3,3), "inputs are not defined on the given domain")
})