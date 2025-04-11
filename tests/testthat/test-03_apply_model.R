

test_that("output", {
  obj <- suppressWarnings({apply_model(obj2)})
  expect_no_error(obj)
  expect_type(obj, type = "list")
  expect_type(obj$stepwise_process , type = "list")
  expect_type(obj$ordered_indep_vars , type = "character")
  expect_type(obj$bootstrap_data , type = "list")

})
