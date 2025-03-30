

test_that("output", {
  obj = bootstrap_data() %>%
    define_indepvars()
  expect_no_error(apply_model(obj))
  expect_type(apply_model(obj), type = "list")
  expect_type(apply_model(obj)$stepwise_process , type = "list")
  expect_type(apply_model(obj)$ordered_indep_vars , type = "charater")
  expect_type(apply_model(obj)$bootstrap_data , type = "list")

})
