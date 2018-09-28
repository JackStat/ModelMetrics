#include <Rcpp.h>
#include <omp.h>
using namespace Rcpp;

// Assumes that actual is sorted by predicted values
// [[Rcpp::export]]
double gini_(NumericVector actual) {

  double n = actual.size();

  double pop_delta = 1/n;
  double total_loss = sum(actual);
  NumericVector accum_loss = actual/total_loss;

  Rcpp::NumericVector giniVector = Rcpp::no_init_vector(n);

  #pragma omp parallel for
  for(int i = 0; i < (int) n; ++i) {
    if(i == 0){
      giniVector[i] = (accum_loss[i] - pop_delta);
    } else {
      giniVector[i] = giniVector[i-1] + (accum_loss[i] - pop_delta);
    }
  }
  double gini = sum(giniVector)/n;
  return gini;

}
