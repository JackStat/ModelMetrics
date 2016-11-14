#' @title Root Mean Squared Log Error
#' @description Calculates the mean square log error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

rmsle <- function(...){
  UseMethod("rmsle")
}

#' @rdname rmsle
#' @export
rmsle.default <- function(actual, predicted){
  rmsle_(actual, predicted)
}

#' @rdname rmsle
#' @export
rmsle.lm <- function(modelObject, ...){

  predicted <- modelObject$fitted.values
  actual <- modelObject$residuals + predicted

  rmsle.default(actual, predicted)
}


#' @rdname rmsle
#' @export
rmsle.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  rmsle.default(actual, predicted)
}

#' @rdname rmsle
#' @export
rmsle.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  rmsle.default(actual, predicted)
}

#' @rdname rmsle
#' @export
rmsle.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  rmsle.default(actual, predicted)
}

#' @rdname rmsle
#' @export
rmsle.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  rmsle.default(actual, predicted)
}

#' @rdname rmsle
#' @export
rmsle.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  rmsle.default(actual, predicted)
}
