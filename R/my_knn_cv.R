#' My k-nearest-neighbors cross-validation
#'
#' This function runs a k-nearest-neighbors classification algorithm on a
#'   specified set of data and performs k-fold cross-validation on it.
#'
#' @param train A data frame of the training data.
#' @param cl A vector of strings containing the true classes of each observation
#'   in \code{train}.
#' @param k_nn A numeric specifying the number of neighbors to be used in the
#'   k-nearest-neighbors algorithm.
#' @param k_cv A numeric specifying the number of folds to be used in the
#'   cross-validation.
#' @keywords prediction
#'
#' @return A list with two elements: \code{class}, a vector of predicted classes
#'   for each observation in \code{train} using \code{k_nn} nearest neighbors
#'   and \code{k_cv} cross-validation folds and \code{cv_error}, the average
#'   misclassification rate of the k-nearest-neighbors algorithm across all
#'   the folds in the cross-validation.
#'
#' @examples
#' my_knn_cv(train = my_penguins %>% select(body_mass_g, bill_length_mm) %>% na.omit(),
#'           cl = my_penguins %>% select(body_mass_g, species) %>% na.omit() %>% select(species),
#'           k_nn = 3,
#'           k_cv = 5)
#'
#' @export
my_knn_cv <- function(train, cl, k_nn, k_cv) {
  # combine classes with obervations
  train_class <- cbind(train, cl)
  # randomly generate folds and assign to each observation
  fold <- sample(rep(1:k_cv, length = nrow(train)))
  train_class <- cbind(train_class, fold)
  # define cumulative misclassification error rate
  misclass_sum <- 0
  # perform k-nearest-neighbors cross-validation on each fold
  for (i in 1:k_cv) {
    # split input data into training and test data based on fold
    temp_train <- train_class %>% filter(fold != i)
    temp_cl <- temp_train$cl
    temp_test <- train_class %>% filter(fold == i)
    temp_result_cl <- temp_test$cl
    # remove cl and fold columns for knn() call
    temp_train$cl <- NULL
    temp_train$fold <- NULL
    temp_test$cl <- NULL
    temp_test$fold <- NULL
    # run knn() on fold i
    predict_class <- knn(train = temp_train,
                         test = temp_test,
                         cl = temp_cl,
                         k = k_nn)
    # add miscalculation rate to cumulative sum
    misclass_sum <- misclass_sum + mean(predict_class != temp_result_cl)
  }
  # calculate average misclassification rate by dividing misclassification
  # proportion sum by number of folds
  avg_misclass_rate <- misclass_sum / k_cv
  # run knn with full data as training and test data
  result_knn <- knn(train = train, test = train, cl = cl, k = k_nn)
  result <- list("class" = result_knn, "cv_error" = avg_misclass_rate)
  return(result)
}
