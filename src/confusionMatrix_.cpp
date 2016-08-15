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
  double Denom = (cMat(1,1) + cMat(1,0));
  double ppv = 0;

  if(Denom != 0){
    ppv = cMat(1,1) / Denom;
  }

  return ppv;

}


// [[Rcpp::export]]
double npv_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);
  double Denom (cMat(0,0) + cMat(0,1));
  double npv = 0;

  if(Denom != 0){
    npv = cMat(0,0) / Denom;
  }

  return npv;

}



// [[Rcpp::export]]
double recall_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);

  double recall = cMat(1,1) / (cMat(1,1) + cMat(0,1));
  return recall;

}

// [[Rcpp::export]]
double f1Score_(NumericVector actual, NumericVector predicted, double cutoff){

  double n = predicted.size();

  double p = ppv_(actual, predicted, cutoff);
  double r = recall_(actual, predicted, cutoff);
  double f1 = 0;

  if(p + r != 0){
    f1 = (2*p*r)/(p + r);
  }

  return f1;
}
