#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double logLoss_(NumericVector actual, NumericVector predicted) {

  NumericVector ll = -1*(actual*log(predicted) + (1-actual)*log(1-predicted));
  double logloss = mean(ll);
  return logloss ;

}
