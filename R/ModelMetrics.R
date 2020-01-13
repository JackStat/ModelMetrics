#' @useDynLib ModelMetrics
#' @importFrom Rcpp sourceCpp
NULL

#' Test data
#'
#' @name testDF
#' @docType data
NULL


#' @title Multiclass Log Loss
#'
#' @description Calculated the multi-class log loss
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted matrix of predicted values. Can be \code{matrix, data.frame}
#'
#' @export

mlogLoss <- function(actual, predicted){

  if(c('factor', 'character') %in% class(actual)){
    actual = as.numeric(as.factor(actual))
  }
  if(c('data.frame') %in% class(predicted)){
    predicted = as.matrix(predicted)
  }

  eps <- 1e-15
  predicted = pmax(pmin(predicted, 1 - eps), eps)

  mlogLoss_(actual, predicted)
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



#' @title Positive Predictive Value
#'
#' @description True Positives / (True Positives + False Positives)
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

#' @title F Score
#' @description Calculates the F score and allows different specifications of the beta value (F0.5)
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#' @param beta the desired beta value (lower increases weight of precision over recall). Defaults to 1
#'
#' @export

fScore <- function(actual, predicted, cutoff = .5, beta = 1){

  fScore_(actual, predicted, cutoff, beta)

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




#' @title kappa statistic
#'
#' @description Calculates kappa statistic. Currently build to handle binary values in \code{actual} vector.
#'
#' @param actual A vector of the labels
#' @param predicted A vector of predicted values
#' @param cutoff A cutoff for the predicted values
#'
#' @export

kappa <- function(actual, predicted, cutoff = .5){
  kappa_(actual, predicted, cutoff)
}

