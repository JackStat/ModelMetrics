#' @useDynLib ModelMetrics
#' @importFrom Rcpp sourceCpp
NULL



#' @title LogLoss
#'
#' @export

logLoss <- function(actual, predicted){
  logLoss_(actual, predicted)
}


#' @title AUC
#'
#' @export

auc <- function(actual, predicted){
  auc_(actual, predicted)
}
