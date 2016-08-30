## ModelMetrics: Rapid Calculation of Model Metrics
[![Build Status](https://travis-ci.org/JackStat/ModelMetrics.svg?branch=master)](https://travis-ci.org/JackStat/ModelMetrics)
[![Build status](https://ci.appveyor.com/api/projects/status/evm55ctrlwp6fjs3/branch/master?svg=true)](https://ci.appveyor.com/project/JackStat/modelmetrics/branch/master)
[![Coverage Status](https://coveralls.io/repos/github/JackStat/ModelMetrics/badge.svg?branch=master)](https://coveralls.io/github/JackStat/ModelMetrics?branch=master)

Tyler Hunt thunt@snapfinance.com

### Introduction
ModelMetrics is a much faster and reliable package for evaluating models. ModelMetrics is written in using Rcpp making it faster than the other packages used for model metrics.


### Installation

You can install this package from CRAN:

```r
install.packages("ModelMetrics")
```

Or you can install the development version from Github with [devtools](https://github.com/hadley/devtools):

```r
devtools::install_github("JackStat/ModelMetrics")
```


### Benchmark and comparison

```r
N = 100000
Actual = as.numeric(runif(N) > .5)
Predicted = as.numeric(runif(N))

actual = Actual
predicted = Predicted

s1 <- system.time(a1 <- ModelMetrics::auc(Actual, Predicted))
s2 <- system.time(a2 <- Metrics::auc(Actual, Predicted))
# Warning message:
# In n_pos * n_neg : NAs produced by integer overflow
s3 <- system.time(a3 <- pROC::auc(Actual, Predicted))
s4 <- system.time(a4 <- MLmetrics::AUC(Predicted, Actual))
# Warning message:
# In n_pos * n_neg : NAs produced by integer overflow
s5 <- system.time({pp <- ROCR::prediction(Predicted, Actual); a5 <- ROCR::performance(pp, 'auc')})


data.frame(
  package = c("ModelMetrics", "pROC", "ROCR")
  ,Time = c(s1[[3]],s3[[3]],s5[[3]])
)

# MLmetrics and Metrics could not calculate so they are dropped from time comparison
#        package   Time
# 1 ModelMetrics  0.030
# 2         pROC 50.359
# 3         ROCR  0.358
```



