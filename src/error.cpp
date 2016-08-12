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
double rmse_(NumericVector actual, NumericVector predicted) {

  double rmse = sqrt(mse_(actual, predicted));
  return rmse;

}

