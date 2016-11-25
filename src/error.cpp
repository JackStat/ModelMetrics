#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mae_(NumericVector actual, NumericVector predicted) {
  double mae = mean(abs(actual - predicted));
  return mae;
}


// [[Rcpp::export]]
double ce_(NumericVector actual, NumericVector predicted) {

  double Rows = predicted.size();
  double ErrorCount = 0;
#pragma omp parallel for
  for(int i = 0; i < Rows; ++i) {
    if(actual(i) != predicted(i)) {
      ErrorCount = ErrorCount + 1;
    }
  }

  double ce = ErrorCount/Rows;
  return ce;

}


// [[Rcpp::export]]
double mse_(NumericVector actual, NumericVector predicted) {

  NumericVector err = (actual-predicted);
  double mse = mean(err*err);
  return mse;

}


// [[Rcpp::export]]
double msle_(NumericVector actual, NumericVector predicted) {

  NumericVector logdiff = (log(1 + actual) - log(1 + predicted));
  NumericVector le = logdiff*logdiff;

  double msle = mean(le);
  return msle;

}


// [[Rcpp::export]]
double rmsle_(NumericVector actual, NumericVector predicted) {

  double rmsle = sqrt(mse_(actual, predicted));
  return rmsle;

}


// [[Rcpp::export]]
double rmse_(NumericVector actual, NumericVector predicted) {

  double rmse = sqrt(mse_(actual, predicted));
  return rmse;

}

