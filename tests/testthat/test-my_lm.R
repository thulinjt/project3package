test_that("my_lm produces correct output", {
  expect_s3_class(my_lm(lifeExp ~ gdpPercap, data = my_gapminder), "table")
})
test_that("my_lm errors when not given proper inputs", {
  expect_error(my_lm(lifeExp ~ gdpPercap))
  expect_error(my_lm(data = my_gapminder))
  expect_error(my_lm())
})
