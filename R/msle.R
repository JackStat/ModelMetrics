#' @title Mean Squared Log Error
#' @description Calculates the mean square log error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{glm, randomForest, glmerMod, gbm}
#'
#' @export

msle <- function(...){
  UseMethod("msle")
}

#' @rdname msle
#' @export
msle.default <- function(actual, predicted, ...){
  msle_(actual, predicted)
}

#' @rdname msle
#' @export
msle.lm <- function(modelObject, ...){

  predicted <- modelObject$fitted.values
  actual <- modelObject$residuals + predicted

  msle.default(actual, predicted)
}


#' @rdname msle
#' @export
msle.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  msle.default(actual, predicted)
}

#' @rdname msle
#' @export
msle.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  msle.default(actual, predicted)
}

#' @rdname msle
#' @export
msle.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  msle.default(actual, predicted)
}

#' @rdname msle
#' @export
msle.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  msle.default(actual, predicted)
}

#' @rdname msle
#' @export
msle.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  msle.default(actual, predicted)
}
