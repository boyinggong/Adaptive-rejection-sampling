context("Derivative of logarithm of functions")

test_that("special cases is correct", {
    print("special cases is correct")
    x <- c(1,2,3)
    expect_equal(dh(x,dexp), c(-1,-1,-1))
})

test_that("special cases is correct", {
    xi <- c(1,2,3)
    expect_equal(dh(xi,dnorm), c(-1,-2,3))
})
