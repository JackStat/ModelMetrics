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

#' @importFrom data.table frankv
#' @rdname auc
#' @export
auc.default <- function(actual, predicted, ...){

  binaryChecks(actual, 'auc')

  if (inherits(actual, 'factor')) {
    actual <- as.integer(actual) - 1L
  } else if (inherits(actual, 'character')) {
    actual <- as.integer(as.factor(actual)) - 1L
  }

  if(length(actual > 10000)){
    ranks = frankv(predicted)
    AUC <- auc3_(actual, predicted, ranks)
  } else {
    AUC <- auc_(actual, predicted)
  }
  return(AUC)
}

#' @rdname auc
#' @export
auc.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  auc.default(actual, predicted)
}

#' @rdname auc
#' @export
auc.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  auc.default(actual, predicted)
}

#' @rdname auc
#' @export
auc.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  auc.default(actual, predicted)
}

#' @rdname auc
#' @export
auc.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  auc.default(actual, predicted)
}

#' @rdname auc
#' @export
auc.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  auc.default(actual, predicted)
}

