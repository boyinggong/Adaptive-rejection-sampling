context("Sk")

test_that("special case are correct", {
  expect_equal(sk(c(-1/2, 1/2), c(-1/2, 1/2), c(-1, 1), c(-1, 0,  1)), c(exp(0)-exp(-1), (exp(1)-exp(0))))
})

test_that("throw error when vector length is not correct", {
  expect_error(sk(c(-1/2, 1/2, 1), c(-1/2, 1/2), c(-1, 1), c(-1, 0,  1)), "length inconsistent")
  expect_error(sk(c(-1/2, 1/2, 1), c(-1/2, 1/2), c(-1, 1), c(-1, 0)), "length inconsistent")
})