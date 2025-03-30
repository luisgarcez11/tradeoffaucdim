library(testthat)

test_that("output", {
  obj = bootstrap_data()
  expect_no_error(define_indepvars(obj))
  expect_type(define_indepvars(obj), type = "list")
  expect_type(define_indepvars(obj)$stepwise_process , type = "list")
  expect_type(define_indepvars(obj)$ordered_indep_vars , type = "character")
  expect_type(define_indepvars(obj)$bootstrap_data , type = "list")

})
