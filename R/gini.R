#' @title GINI Coefficient
#'
#' @description Calculates the GINI coefficient for a binary classifcation model
#'
#' @param actual A vector of the labels. Can be \code{numeric, character, or factor}
#' @param predicted A vector of predicted values
#' @param \dots additional parameters to be passed the the s3 methods
#' @param modelObject the model object. Currently supported \code{glm, randomForest, glmerMod, gbm}
#'
#' @examples
#' data(testDF)
#' glmModel <- glm(y ~ ., data = testDF, family="binomial")
#' Preds <- predict(glmModel, type = 'response')
#'
#' gini(testDF$y, Preds)
#'
#' @export

gini <- function(...){
  UseMethod("gini")
}

#' @rdname gini
#' @importFrom data.table fsort
#' @export
gini.default <- function(actual, predicted, ...){

  df1 <- data.frame(actual = actual, predicted = predicted)
  df2 <- data.frame(actual = actual, predicted = actual)
  df1 <- df1[order(-df1$predicted),]
  df2 <- df2[order(-df2$actual),]
  gini <- gini_(df1$actual)/gini_(df2$actual)
  return(gini)
}

#' @rdname gini
#' @export
gini.glm <- function(modelObject, ...){

  family <- family(modelObject)[[1]]
  if(any(family %in% c('binomial', 'poisson'))){
    actual <- modelObject$y
    predicted <- modelObject$fitted.values
  } else {
    stop(paste0("family: ", family, " is not currently supported"))
  }

  gini.default(actual, predicted)
}

#' @importFrom stats predict
#' @rdname gini
#' @export
gini.randomForest <- function(modelObject, ...){

  actual <- as.numeric(modelObject$y) - 1
  predicted <- predict(modelObject, type = 'prob')[,2]

  gini.default(actual, predicted)
}

#' @rdname gini
#' @export
gini.glmerMod <- function(modelObject, ...){

  actual <- modelObject@resp$y
  predicted <- modelObject@resp$mu

  gini.default(actual, predicted)
}

#' @rdname gini
#' @export
gini.gbm <- function(modelObject, ...){

  actual <- modelObject$data$y
  predicted <- modelObject$fit

  gini.default(actual, predicted)
}

#' @rdname gini
#' @export
gini.rpart <- function(modelObject, ...){

  actual <- modelObject$y
  predicted <- predict(modelObject)

  gini.default(actual, predicted)
}
