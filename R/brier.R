#' @title Brier Score
#' @description Calculates the Brier score
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

brier <- function(...){
  UseMethod("brier")
}

#' @rdname brier
#' @export
brier.default <- function(actual, predicted, ...){
  brier_(actual, predicted)
}


#' @rdname brier
#' @export
brier.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  brier.default(actual, predicted)
}

#' @rdname brier
#' @export
brier.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  brier.default(actual, predicted)
}

#' @rdname brier
#' @export
brier.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  brier.default(actual, predicted)
}

#' @rdname brier
#' @export
brier.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  brier.default(actual, predicted)
}

#' @rdname brier
#' @export
brier.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  brier.default(actual, predicted)
}

