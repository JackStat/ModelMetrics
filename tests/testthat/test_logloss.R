
context("logLoss Tests")

test_that("mlogLoss character/factor actual", {

  Levs = 8
  Size = 100
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)

  m1 <- mlogLoss(y, xm)
  m2 <- mlogLoss(as.character(y), xm)
  m3 <- mlogLoss(as.factor(y), xm)

  expect_true(m1 == m2)
  expect_true(m1 == m3)

})


test_that("mlogLoss data.frame predicted", {

  Levs = 8
  Size = 100
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)

  m1 <- mlogLoss(y, xm)
  m2 <- mlogLoss(as.character(y), data.frame(xm))

  expect_true(m1 == m2)

})


test_that("logLoss estimates with 0s and 1s as values", {

  data(testDF)
  glmModel <- glm(y ~ ., data=testDF, family="binomial")
  Preds <- predict(glmModel, type = 'response')

  Preds[1] = 0
  Preds[2] = 1
  logLoss(testDF$y, Preds)

})
