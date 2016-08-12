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



#' @title Confusion Matrix
#' @description Create a confusion matrix given a specific cutoff.
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

confusionMatrix <- function(actual, predicted, cutoff = .5){
  confusionMatrix_(actual, predicted, cutoff)
}



#' @title Postive Predictive Value
#'
#' @description True Postives / (True Positives + False Positives)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

ppv <- function(actual, predicted, cutoff = .5){
  ppv_(actual, predicted, cutoff)
}



#' @title Negative Predictive Value
#'
#' @description True Negatives / (True Negatives + False Negatives)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

npv <- function(actual, predicted, cutoff = .5){
  npv_(actual, predicted, cutoff)
}



#' @title Recall
#'
#' @description True Positives / (True Positives + False Negatives)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

recall <- function(actual, predicted, cutoff = .5){
  recall_(actual, predicted, cutoff)
}



#' @title Mean F1 Score
#' @description Calculates the mean f1 score
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

f1Score1 <- function(actual, prediction){

  f1Score_(actual, prediction)

}



#' @title Mean absolute error
#' @description Calculates the mean absolute error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

mae <- function(actual, prediction){

  mae_(actual, prediction)

}



#' @title Classification error
#' @description Calculates the classification error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

ce <- function(actual, predicted){

  ce_(actual, predicted)

}




#' @title Mean Squared Log Error
#' @description Calculates the mean square log error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

msle <- function(actual, predicted){
  msle_(actual, predicted)
}




#' @title Root Mean Squared Log Error
#' @description Calculates the mean square log error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

rmsle <- function(actual, predicted){
  rmsle_(actual, predicted)
}
