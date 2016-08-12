#' @useDynLib ModelMetrics
#' @importFrom Rcpp sourceCpp
NULL



#' @title Log Loss
#'
#' @description Calculates the log loss or entropy loss for a binary outcome
#'
#' @param actual a binary vector of the labels
#' @param predicted a vector of predicted values
#' @param distribution the distribution of the loss function needed \code{binomial, poisson}
#'
#' @export

logLoss <- function(actual, predicted, distribution = "binomial"){

  if(distribution == "binomial"){

    return(logLoss_(actual, predicted))

  } else if(distribution == 'poisson'){

    return(plogLoss_(actual, predicted))

  } else {
    stop(paste(distribution, "is not defined. Please use binomial or poisson"))
  }

}


#' @title Multiclass Log Loss
#'
#' @description Calculated the multi-class log loss
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted matrix of predicted values
#'
#' @export

mlogLoss <- function(actual, predicted){

  if(class(actual) %in% c('factor', 'character')){
    actual = as.numeric(as.factor(actual))
  }
  mlogLoss_(actual, predicted)
}



#' @title Area Under the Curve
#'
#' @description Calculates the area under the curve for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A vector of predicted values
#'
#' @export

auc <- function(actual, predicted){
  auc_(actual, predicted)
}


#' @title Mean Square Error
#' @description Calculates the mean square error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

mse <- function(actual, predicted){
  mse_(actual, predicted)
}


#' @title Root-Mean Square Error
#' @description Calculates the root mean square error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

rmse <- function(actual, predicted){
  rmse_(actual, predicted)
}


#' @title Average absolute error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param type use \code{absolute} to calculate absolute error and \code{class} for classification error
#'
#' @export

error <- function(actual, prediction, type = 'absolute'){

  if(type == 'absolute'){
    err = ae_(actual, prediction)
  } else if(type == 'class') {
    err = ce_(actual, prediction)
  } else {
    stop(paste0(type, "is not a defined type use abolute or class"))
  }

  return(err)

}

