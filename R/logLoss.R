#' @title Log Loss
#'
#' @description Calculates the log loss or entropy loss for a binary outcome
#'
#' @param actual a binary vector of the labels
#' @param predicted a vector of predicted values
#' @param distribution the distribution of the loss function needed \code{binomial, poisson}
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' logLoss(testDF$y, Preds)
#'
#' @export

logLoss <- function(...){
  UseMethod("logLoss")
}

#' @export
logLoss.default <- function(actual, predicted, distribution = "binomial"){

  eps <- 1e-15
  predicted = pmax(pmin(predicted, 1 - eps), eps)

  if(distribution == "binomial"){

    return(logLoss_(actual, predicted))

  } else if(distribution == 'poisson'){

    return(plogLoss_(actual, predicted))

  } else {
    stop(paste(distribution, "is not defined. Please use binomial or poisson"))
  }

}

#' @export
logLoss.glm <- function(modelObject){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  logLoss.default(actual, predicted, distribution = family)
}

#' @export
logLoss.randomForest <- function(modelObject){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  logLoss.default(actual, predicted)
}


#' @export
logLoss.glmerMod <- function(modelObject){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  logLoss.default(actual, predicted)
}

