% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ModelMetrics.R
\name{confusionMatrix}
\alias{confusionMatrix}
\title{Confusion Matrix}
\usage{
confusionMatrix(actual, predicted, cutoff = 0.5, use_names = FALSE)
}
\arguments{
\item{actual}{A vector of the labels}

\item{predicted}{A vector of predicted values}

\item{cutoff}{A cutoff for the predicted values}

\item{use_names}{If \code{TRUE}, the dimnames of the output matrix will be labelled to indicate
which axes show the actual and predicted classes.}
}
\description{
Create a confusion matrix given a specific cutoff.
}
\examples{
actual <- c(0, 0, 1)
predicted <- c(1, 0, 1)

confusionMatrix(actual, predicted)

confusionMatrix(actual, predicted, use_names = TRUE)

}
