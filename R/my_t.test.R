#' My t-test
#'
#' This function performs a t-test on a set of data against a hypothesis.
#'
#' @param x Numeric vector of data on which t-test is performed.
#' @param alternative String input that specifies the type of t-test to be
#'   performed on \code{x}, must be either \code{"less"} or \code{"greater"}
#'   for a one-sided t-test or \code{"two.sided"} for a two-sided test.
#' @param mu Numeric representing the null hypothesis value of the mean.
#' @keywords inference
#'
#' @return List with four elements: \code{test_stat} is a numeric contianing
#'   the value of the test statistic of \code{x}, \code{df} is a numeric
#'   containing the degrees of freedom used in the t-test, \code{alternative}
#'   is a string that represents the type of t-test used (the value of the
#'   \code{alternative} parameter), and \code{p-val} is a numeric containing
#'   the p-value for the t-test.
#'
#' @examples
#' my_t.test(x = my_gapminder$lifeExp, alternative = "two.sided", mu = 55)
#' my_t.test(x = my_gapminder$gdpPercap, alternative = "greater", mu = 10000)
#'
#' @export
my_t.test <- function(x, alternative, mu) {
  # calculate test statistic
  test_stat <- (mean(x) - mu) / (sd(x) / sqrt(length(x)))

  # calculate df
  df <- length(x) - 1

  # check test type
  if (alternative == "less") {
    # one-sided test, lower tail
    p_val <- pt(test_stat, df, lower.tail = TRUE)
  } else if (alternative == "greater") {
    # one-sided test, upper tail
    p_val <- pt(test_stat, df, lower.tail = FALSE)
  } else if (alternative == "two.sided") {
    # two-sided test
    p_val <- pt(abs(test_stat), df, lower.tail = FALSE) * 2
  } else {
    stop("alternative must be \"less,\" \"greater,\" or \"two.sided\"")
  }

  result <- list("test_stat" = test_stat,
                 "df" = df,
                 "alternative" = alternative,
                 "p_val" = p_val)
  return(result)
}
