
test_that("output", {
  obj = bootstrap_data() %>%
    define_indepvars() %>%
    apply_model()
  expect_no_error(summary_stats(obj))
  expect_type(summary_stats(obj), type = "list")
  expect_type(summary_stats(obj)$stepwise_process , type = "list")
  expect_type(summary_stats(obj)$ordered_indep_vars , type = "character")
  expect_type(summary_stats(obj)$bootstrap_data , type = "list")
  expect_type(summary_stats(obj)$summary_stats , type = "list")
  expect_type(summary_stats(obj)$models , type = "character")

})

