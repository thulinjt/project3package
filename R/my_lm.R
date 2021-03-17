#' My Linear Model
#'
#' This function calculates a linear regression model for given data.
#'
#' @param formula A formula object with the linear regression equation to be
#'   calculated from data set \code{data}.
#' @param data A data frame containing the data to be modeled.
#' @keywords inference
#' @keywords prediction
#'
#' @return A table with coefficient estimates and standard errors for each
#'   variable specified by \code{formula} in the linear model as well as
#'   t-values and p-values for two-sided t-tests performed on each coefficient
#'   estimate.
#'
#' @export
my_lm <- function(formula, data) {
  # obtain model matrix
  lm_matrix <- model.matrix(formula, data)
  # obtain model response
  lm_response <- model.response(model.frame(formula, data)) %>% as.vector()
  # calculate model coefficient estimates
  lm_coeff <- solve(t(lm_matrix) %*% lm_matrix) %*%
    t(lm_matrix) %*% lm_response

  # calculate degrees of freedom
  lm_df <- nrow(lm_matrix) - length(lm_coeff)

  # calulate variance estimate
  lm_var <- sum(((lm_response - as.vector(lm_matrix %*% lm_coeff)) ^ 2) / lm_df)
  # calculate standard error
  lm_se <- sqrt(diag(solve(t(lm_matrix) %*% lm_matrix) * lm_var))

  # calculate t-statistic value
  lm_t_val <- lm_coeff / lm_se
  # calculate p-value for two-sided t-test
  lm_t_prob <- pt(abs(lm_t_val), lm_df, lower.tail = FALSE) * 2

  result <- cbind(lm_coeff, lm_se, lm_t_val, lm_t_prob)
  # rename columns in result
  colnames(result) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  return(as.table(result))
}
