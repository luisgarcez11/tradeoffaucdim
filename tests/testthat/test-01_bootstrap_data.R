library(testthat)

test_that("output", {
  expect_no_error(bootstrap_data())
  expect_error(bootstrap_data(outcome = "test"))
  expect_error(bootstrap_data(data = mtcars %>%
                                mutate(vs = 1:nrow(mtcars))))
  expect_type(bootstrap_data(), type = "list")

})
