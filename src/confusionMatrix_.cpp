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
double tnr_(NumericVector actual, NumericVector predicted, double cutoff) {

  double TN = sum(predicted < cutoff & actual == 0);
  double N = sum(actual == 0);
  double tnr = TN/N;

  return tnr;

}



// [[Rcpp::export]]
double recall_(NumericVector actual, NumericVector predicted, double cutoff) {

  NumericMatrix cMat = confusionMatrix_(actual, predicted, cutoff);

  double recall = cMat(1,1) / (cMat(1,1) + cMat(0,1));
  return recall;

}

// [[Rcpp::export]]
double f1Score_(NumericVector actual, NumericVector predicted, double cutoff){

  double p = ppv_(actual, predicted, cutoff);
  double r = recall_(actual, predicted, cutoff);
  double f1 = 0;

  if(p + r != 0){
    f1 = (2*p*r)/(p + r);
  }

  return f1;
}


// [[Rcpp::export]]
double brier_(NumericVector actual, NumericVector predicted){

  double brier = mean(pow(actual - predicted, 2));
  return brier;
}


// [[Rcpp::export]]
double mcc_(NumericVector actual, NumericVector predicted, double cutoff){

  // True Negatives
  double TN = sum(predicted < cutoff & actual == 0);
  // False Negatives
  double FN = sum(predicted < cutoff & actual == 1);
  // False positives
  double FP = sum(predicted >= cutoff & actual == 0);
  // True positives
  double TP = sum(predicted >= cutoff & actual == 1);

  double numerator = ((TP*TN) - (FP*FN));
  double denom = sqrt((TP + FP)*(TP + FN)*(TN + FP)*(TN + FN));

  double mcc = numerator/denom;
  return mcc;
}


// [[Rcpp::export]]
double kappa_(NumericVector actual, NumericVector predicted, double cutoff){

  // True Negatives d
  double TN = sum(predicted < cutoff & actual == 0);
  // False Negatives - c
  double FN = sum(predicted < cutoff & actual == 1);
  // False positives - b
  double FP = sum(predicted >= cutoff & actual == 0);
  // True positives - a
  double TP = sum(predicted >= cutoff & actual == 1);

  double N = (TP + FP + FN + TN);

  double po = (TP + TN)/N;
  double margin_a = ((TP + FP)*(TP + FN))/N;
  double margin_b = ((FN + TN)*(FP + TN))/N;

  double pe = (margin_a + margin_b)/N;

  double kappa = (po - pe)/(1 - pe);
  return kappa;
}

