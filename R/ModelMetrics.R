#' @useDynLib ModelMetrics
#' @importFrom Rcpp sourceCpp
NULL

#' Test data
#'
#' @name testDF
#' @docType data
NULL

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

logLoss <- function(actual, predicted, distribution = "binomial"){

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


#' @title Multiclass Log Loss
#'
#' @description Calculated the multi-class log loss
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted matrix of predicted values. Can be \code{matrix, data.frame}
#'
#' @export

mlogLoss <- function(actual, predicted){

  if(class(actual) %in% c('factor', 'character')){
    actual = as.numeric(as.factor(actual))
  }
  if(class(predicted) %in% c('data.frame')){
    predicted = as.matrix(predicted)
  }

  eps <- 1e-15
  predicted = pmax(pmin(predicted, 1 - eps), eps)

  mlogLoss_(actual, predicted)
}



#' @title Area Under the Curve
#'
#' @description Calculates the area under the curve for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A vector of predicted values
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' auc(testDF$y, Preds)
#'
#' @export

auc <- function(actual, predicted){

  binaryChecks(actual, 'auc')
  if(class(actual) %in% c('factor', 'character')){
    actual = as.numeric(as.factor(as.character(actual))) - 1
  }

  auc_(actual, predicted)
}


#' @title Multiclass Area Under the Curve
#'
#' @description Calculates the area under the curve for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A data.frame of predicted values. Can be \code{matrix, data.frame}
#'
#'
#' @examples
#' setosa <- glm(I(Species == 'setosa') ~ Sepal.Length, data = iris, family = 'binomial')
#' versicolor <- glm(I(Species == 'versicolor') ~ Sepal.Length, data = iris, family = 'binomial')
#' virginica <- glm(I(Species == 'virginica') ~ Sepal.Length, data = iris, family = 'binomial')
#'
#' Pred <-
#'   data.frame(
#'     setosa = predict(setosa, type = 'response')
#'     ,versicolor = predict(versicolor, type = 'response')
#'     ,virginica = predict(virginica, type = 'response')
#'   )
#'
#' Predicted = Pred/rowSums(Pred)
#' Actual = iris$Species
#'
#' mauc(Actual, Predicted)
#'
#' @export

mauc <- function(actual, predicted){

  actual <- factor(actual)
  Data <- data.frame(predicted, actual)
  Outcomes <- length(unique(actual))

  simpleAUC <- function(x){
    # One-vs-all
    y1 = levels(Data$actual)[x]
    y  <- as.numeric(Data[, "actual"] == y1)
    prob <- Data[,x]
    AUCs <- auc(y, prob)
    return(AUCs)
  }

  AUCs <- sapply(1:Outcomes, simpleAUC)
  list(mauc = mean(AUCs), auc = AUCs)

}


#' @title Mean Square Error
#' @description Calculates the mean square error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' mse(testDF$y, Preds)
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
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' rmse(testDF$y, Preds)
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
#' @aliases precision
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' ppv(testDF$y, Preds, cutoff = 0)
#' precision(testDF$y, Preds, cutoff = 0)
#'
#' @export

ppv <- function(actual, predicted, cutoff = .5){
  ppv_(actual, predicted, cutoff)
}

#' @export

precision <- function(actual, predicted, cutoff = .5){
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
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' npv(testDF$y, Preds, cutoff = 0)
#'
#' @export

npv <- function(actual, predicted, cutoff = .5){
  npv_(actual, predicted, cutoff)
}



#' @title Recall, Sensitivity, tpr
#'
#' @aliases sensitivity tpr
#'
#' @description True Positives / (True Positives + False Negatives)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' recall(testDF$y, Preds, cutoff = 0)
#' sensitivity(testDF$y, Preds, cutoff = 0)
#' tpr(testDF$y, Preds, cutoff = 0)
#'
#' @export

recall <- function(actual, predicted, cutoff = .5){
  recall_(actual, predicted, cutoff)
}

#' @export
sensitivity <- function(actual, predicted, cutoff = .5){
  recall_(actual, predicted, cutoff)
}

#' @export
tpr <- function(actual, predicted, cutoff = .5){
  recall_(actual, predicted, cutoff)
}


#' @title Specificity, True negative rate
#'
#' @aliases specificity tnr
#'
#' @description True Negatives / (True Negatives + False Positives)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' tnr(testDF$y, Preds, cutoff = 0)
#' specificity(testDF$y, Preds, cutoff = 0)
#'
#' @export

tnr <- function(actual, predicted, cutoff = .5){
  tnr_(actual, predicted, cutoff)
}

#' @export
specificity <- function(actual, predicted, cutoff = .5){
  tnr_(actual, predicted, cutoff)
}



#' @title F1 Score
#' @description Calculates the f1 score
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

f1Score <- function(actual, predicted, cutoff = .5){

  f1Score_(actual, predicted, cutoff)

}



#' @title Mean absolute error
#' @description Calculates the mean absolute error
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

mae <- function(actual, predicted){

  mae_(actual, predicted)

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



#' @title Brier Score
#' @description Calculates the Brier score
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#'
#' @export

brier <- function(actual, predicted){
  brier_(actual, predicted)
}




#' @title Matthews Correlation Coefficient
#' @description Calculates the Matthews Correlation Coefficient
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

mcc <- function(actual, predicted, cutoff){
  mcc_(actual, predicted, cutoff)
}


