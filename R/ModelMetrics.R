#' @useDynLib ModelMetrics
#' @importFrom Rcpp sourceCpp
NULL



#' @title Log Loss
#'
#' @export

logLoss <- function(actual, predicted){
  logLoss_(actual, predicted)
}


#' @title Multiclass Log Loss
#'
#' @export

mlogLoss <- function(actual, predicted){
  mlogLoss_(actual, predicted)
}



#' @title AUC
#'
#' @export

auc <- function(actual, predicted){
  auc_(actual, predicted)
}


#' @title MSE
#'
#' @export

mse <- function(actual, predicted){
  mse_(actual, predicted)
}
