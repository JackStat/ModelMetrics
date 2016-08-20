context("Error Messages")


test_that("Error messages are correct", {

  Actual = sample(c(0,1), 10, replace = TRUE)
  Predicted = runif(10)
  expect_error(logLoss(Actual, Predicted, distribution = "exp")
               ,'exp is not defined. Please use binomial or poisson')

  expect_silent(logLoss(Actual, Predicted, distribution = "binomial"))

  expect_silent(logLoss(Actual, Predicted, distribution = "poisson"))

})
