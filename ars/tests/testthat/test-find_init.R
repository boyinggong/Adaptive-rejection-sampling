context("Test the find_init function")

test_that("special cases are correct", {
  my_chisq <- function(x){dchisq(x,df=4)}
  neg_chisq <- function(x){dchisq(-x,df=4)}
  my_dt <- function(x){dt(x,df=4)}
  my_df <- function(x){df(x, 2, 3)}
  
  print("left bounded, right unbounded") # derivative of right starting point smaller than 0
  expect_true(dh(find_init(dnorm, c(1, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(dnorm, c(-1, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(my_dt, c(1, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(my_dt, c(-1, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(my_chisq, c(0, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(dexp, c(0,Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(my_df, c(0,Inf))[2], dnorm) < 0)
  
  print("left unbounded, right bounded") # derivative of left starting point greater than 0
  expect_true(dh(find_init(dnorm, c(-Inf, -1))[1], dnorm) > 0)
  expect_true(dh(find_init(dnorm, c(-Inf, 1))[1], dnorm) > 0)
  expect_true(dh(find_init(my_dt, c(-Inf, -1))[1], dnorm) > 0)
  expect_true(dh(find_init(my_dt, c(-Inf, 1))[1], dnorm) > 0)
  expect_true(dh(find_init(neg_chisq, c(-Inf, 0))[1], dnorm) > 0)
  
  print("left unbounded, right unbounded") # derivative of right starting point smaller than 0, derivative of left starting point greater than 0
  expect_true(dh(find_init(dnorm, c(-Inf, Inf))[1], dnorm) > 0)
  expect_true(dh(find_init(dnorm, c(-Inf, Inf))[2], dnorm) < 0)
  expect_true(dh(find_init(my_dt, c(-Inf, Inf))[1], dnorm) > 0)
  expect_true(dh(find_init(my_dt, c(-Inf, Inf))[2], dnorm) < 0)
  
#   print("left bounded, right bounded")
#   find_init(dnorm, c(1, 4))
})