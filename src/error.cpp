#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double ae_(NumericVector actual, NumericVector predicted) {
  double ae = mean(abs(actual - predicted));
  return ae;
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
