#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double auc_(NumericVector actual, NumericVector predicted, NumericVector ranks) {

  double n = actual.size();

  double NPos = sum(actual == 1);
  double NNeg = (actual.size() - NPos);

  double sumranks = 0;

  for(int i = 0; i < n; ++i) {
    if (actual[i] == 1){
      sumranks = sumranks + ranks[i];
    }
  }

  double p1 = (sumranks - NPos*( NPos + 1 ) / 2);
  double p2 = NPos*NNeg;

  double auc =  p1 / p2;
  return auc ;

}


