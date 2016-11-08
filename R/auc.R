#' @title Area Under the Curve
#'
#' @description Calculates the area under the curve for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{glm, randomForest, glmerMod, gbm}
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' auc(testDF$y, Preds)
#'
#' @export

auc <- function(...){
  UseMethod("auc")
}

#' @export
auc.default <- function(actual, predicted){

  binaryChecks(actual, 'auc')
  if(class(actual) %in% c('factor', 'character')){
    actual = as.numeric(as.factor(as.character(actual))) - 1
  }

  auc_(actual, predicted)
}

#' @export
auc.glm <- function(modelObject){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  auc.default(actual, predicted)
}

#' @export
auc.randomForest <- function(modelObject){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  auc.default(actual, predicted)
}

#' @export
auc.glmerMod <- function(modelObject){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  auc.default(actual, predicted)
}

#' @export
auc.gbm <- function(modelObject){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  auc.default(actual, predicted)
}

