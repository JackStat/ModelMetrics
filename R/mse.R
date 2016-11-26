#' @title Mean Square Error
#' @description Calculates the mean square error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{lm}
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' mse(testDF$y, Preds)
#'
#' @export

mse <- function(...){
  UseMethod("mse")
}

#' @rdname mse
#' @export
mse.default <- function(actual, predicted, ...){
  mse_(actual, predicted)
}

#' @rdname mse
#' @export
mse.lm <- function(modelObject, ...){

  predicted <- modelObject$fitted.values
  actual <- modelObject$residuals + predicted

  mse.default(actual, predicted)
}

#' @rdname mse
#' @export
mse.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  mse.default(actual, predicted)
}


#' @title Root-Mean Square Error
#' @description Calculates the root mean square error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{lm}
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' rmse(testDF$y, Preds)
#'
#' @export


rmse <- function(...){
  UseMethod("rmse")
}

#' @rdname rmse
#' @export
rmse.default <- function(actual, predicted, ...){
  rmse_(actual, predicted)
}

#' @rdname rmse
#' @export
rmse.lm <- function(modelObject, ...){

  predicted <- modelObject$fitted.values
  actual <- modelObject$residuals + predicted

  rmse.default(actual, predicted)
}

#' @rdname rmse
#' @export
rmse.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  rmse.default(actual, predicted)
}






