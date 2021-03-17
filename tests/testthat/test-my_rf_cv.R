test_that("my_rf_cv gives correct output type", {
  expect_type(my_rf_cv(3), "double")
})
test_that("my_rf_cv throws error if input is incorrect type", {
  expect_error(my_rf_cv("string"))
})
