#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix confusionMatrix_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = NumericMatrix(Dimension(2, 2));

  // True Negatives
  cMat(0,0) = sum(predicted <= cutoff & actual == 0);
  // False Negatives
  cMat(0,1) = sum(predicted <= cutoff & actual == 1);
  // False positives
  cMat(1,0) = sum(predicted > cutoff & actual == 0);
  // True positives
  cMat(1,1) = sum(predicted > cutoff & actual == 1);

  return cMat;

}

// [[Rcpp::export]]
double ppv_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);

  double ppv = cMat(1,1) / (cMat(1,1) + cMat(1,0));
  return ppv;

}


// [[Rcpp::export]]
double npv_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);

  double npv = cMat(0,0) / (cMat(0,0) + cMat(0,1));
  return npv;

}



// [[Rcpp::export]]
double recall_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);

  double recall = cMat(1,1) / (cMat(1,1) + cMat(0,1));
  return recall;

}

// [[Rcpp::export]]
double f1Score_(NumericVector actual, NumericVector predicted){

  double n = predicted.size();
  NumericVector f1 = NumericVector(n);

  for(int i = 0; i < n; ++i) {
    if (actual[i] == 1){
      double p = ppv_(actual, predicted, predicted(i));
      double r = recall_(actual, predicted, predicted(i));
      f1(i) = (2*p*r)/(p + r);
    }
  }

  double f1score = mean(f1);
  return f1score;
}
