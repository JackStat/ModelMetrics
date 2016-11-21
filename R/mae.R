#' @title Mean absolute error
#' @description Calculates the mean absolute error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

mae <- function(...){
  UseMethod("mae")
}

#' @rdname mae
#' @export
mae.default <- function(actual, predicted, ...){
  mae_(actual, predicted)
}


#' @rdname mae
#' @export
mae.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  mae.default(actual, predicted)
}

#' @rdname mae
#' @export
mae.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  mae.default(actual, predicted)
}

#' @rdname mae
#' @export
mae.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  mae.default(actual, predicted)
}

#' @rdname mae
#' @export
mae.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  mae.default(actual, predicted)
}

#' @rdname mae
#' @export
mae.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  mae.default(actual, predicted)
}

