
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


test_that("rmse returns correct values", {

  expect_equal(rmse(testDF$y, Preds), 0.2188343, tolerance = .000001)

})


test_that("mse returns correct values", {

  expect_equal(mse(testDF$y, Preds), 0.04788846, tolerance = .000001)

})


test_that("ppv returns correct values", {

  expect_equal(ppv(testDF$y, Preds, .5), 0.9365079, tolerance = .000001)

})


test_that("npv returns correct values", {

  expect_equal(npv(testDF$y, Preds, .5), 0.9189189, tolerance = .000001)

})


test_that("sensitivity returns correct values", {

  expect_equal(sensitivity(testDF$y, Preds, .5), 0.9516129, tolerance = .000001)

})


