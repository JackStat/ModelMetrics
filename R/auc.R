#' @title Area Under the Curve
#'
#' @description Calculates the area under the curve for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A vector of predicted values
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
