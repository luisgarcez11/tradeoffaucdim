

test_that("output", {
  obj = bootstrap_data() %>%
    define_indepvars() %>%
    apply_model() %>%
    summary_stats()
  expect_no_error(plot_curve(obj))
  expect_type(plot_curve(obj), type = "list")
  expect_type(plot_curve(obj)$stepwise_process , type = "list")
  expect_type(plot_curve(obj)$ordered_indep_vars , type = "character")
  expect_type(plot_curve(obj)$bootstrap_data , type = "list")
  expect_type(plot_curve(obj)$summary_stats , type = "list")
  expect_type(plot_curve(obj)$models , type = "character")
  expect_type(plot_curve(obj)$plot , type = "list")
})


