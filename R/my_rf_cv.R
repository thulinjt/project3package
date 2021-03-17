#' My random forest cross validation
#'
#' This function calculates the cross-validation error of a k-fold
#'   cross-validation of a 100-branch random forest algorithm performed on three variables
#'   of the \code{penguins} data.
#'
#' @param k A numeric containing the number of folds to use in the k-fold
#'   cross-validation.
#' @keywords prediction
#'
#' @return A numeric representing the average mean-squared-error across all
#'   \code{k} folds.
#'
#' @export
my_rf_cv <- function(k) {
  # obtain desired penguins data
  fun_data <- my_penguins %>%
    dplyr::select(body_mass_g, bill_length_mm, bill_depth_mm, flipper_length_mm) %>%
    na.omit()
  # randomly assign observations to one of k folds
  fold <- sample(rep(1:k, length = nrow(fun_data)))
  fun_data <- cbind(fun_data, fold)
  # define cumulative sum of mean squared errors for each fold
  mse_sum <- 0
  # iterate through k folds
  for (i in 1:k) {
    # split data in fold and out of fold
    temp_train <- fun_data %>% filter(fold != i)
    temp_test <- fun_data %>% filter(fold == i)
    # run randomForest() on training data outside of fold
    rf_model <- randomForest::randomForest(formula = body_mass_g ~ bill_length_mm +
                               bill_depth_mm +
                               flipper_length_mm,
                             data = temp_train,
                             ntree = 100)
    # predict body mass for data in fold using out-of-fold model
    model_predict <- predict(rf_model, temp_test[, -1])
    # add MSE for fold i to cumulative MSE sum
    mse_sum <- mse_sum + mean((temp_test$body_mass_g - model_predict)^2)
  }
  # calculate average MSE across all folds
  mean_mse <- mse_sum / k
  return(mean_mse)
}
