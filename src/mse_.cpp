#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mse_(NumericVector actual, NumericVector predicted) {

  NumericVector err = (actual-predicted);
  double mse = mean(err*err);
  return mse;

}
