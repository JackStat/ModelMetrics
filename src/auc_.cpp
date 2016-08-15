#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
NumericVector sort_rcpp(NumericVector x) {
  std::vector<double> tmp = Rcpp::as< std::vector<double> > (x);
  std::sort(tmp.begin(), tmp.end());
  return wrap(tmp);
}

// [[Rcpp::export]]
IntegerVector rank(NumericVector x) {
  return match(x, sort_rcpp(x));
}

// [[Rcpp::export]]
double auc_(NumericVector actual, NumericVector predicted) {

  double n = actual.size();

  IntegerVector Ranks = rank(predicted);
  double NPos = sum(actual == 1);
  double NNeg = (actual.size() - NPos);

  double sumranks = 0;

  for(int i = 0; i < n; ++i) {
    if (actual[i] == 1){
      sumranks = sumranks + Ranks[i];
    }
  }

  double p1 = (sumranks - NPos*( NPos + 1 ) / 2);
  double p2 = NPos*NNeg;

  double auc =  p1 / p2;
  return auc ;

}


