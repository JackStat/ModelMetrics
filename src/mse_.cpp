#include <Rcpp.h>
using namespace Rcpp;

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

