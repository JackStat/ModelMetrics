
context("Calculation Tests")
data(testDF)
glmModel <- glm(y ~ ., data=testDF, family="binomial")
Preds <- predict(glmModel, type = 'response')

test_that("logLoss returns correct values", {

  expect_equal(logLoss(testDF$y, Preds), 0.1546854, tolerance = .000001)

})

test_that("auc returns correct values", {

  expect_equal(auc(testDF$y, Preds), 0.9872666, tolerance = .000001)

})

test_that("auc returns correct values", {

  expect_equal(auc(testDF$y, Preds), 0.9872666, tolerance = .000001)

})
