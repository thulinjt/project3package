test_that("my_t.test provides correct output type", {
  expect_type(my_t.test(x = c(1, 2, 2, 3, 4, 2, 3, 2, 4, 2),
                               alternative = "two.sided",
                               mu = 1.5), "list")
  expect_equal(length(my_t.test(x = c(1, 2, 2, 3, 4, 2, 3, 2, 4, 2),
                         alternative = "two.sided",
                         mu = 1.5)), 4)
})
test_that("my_t.test throws error if parameter not specified", {
  expect_error(my_t.test(x = c(1, 2, 2, 3, 4, 2, 3, 2, 4, 2),
                         mu = 1.5))
  expect_error(my_t.test(x = c(1, 2, 2, 3, 4, 2, 3, 2, 4, 2),
                         alternative = "twosided",
                         mu = 1.5))
})
