#' @title Classification error
#' @description Calculates the classification error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{lm, glm, randomForest, glmerMod, gbm, rpart}
#'
#' @export

ce <- function(...){
  UseMethod("ce")
}

#' @rdname ce
#' @export
ce.default <- function(actual, predicted, ...){
  ce_(actual, predicted)
}

#' @rdname ce
#' @export
ce.lm <- function(modelObject, ...){

  predicted <- modelObject$fitted.values
  actual <- modelObject$residuals + predicted

  ce.default(actual, predicted)
}


#' @rdname ce
#' @export
ce.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  ce.default(actual, predicted)
}

#' @rdname ce
#' @export
ce.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  ce.default(actual, predicted)
}

#' @rdname ce
#' @export
ce.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  ce.default(actual, predicted)
}

#' @rdname ce
#' @export
ce.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  ce.default(actual, predicted)
}

#' @rdname ce
#' @export
ce.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  msle.default(actual, predicted)
}
