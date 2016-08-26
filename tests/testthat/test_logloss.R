
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


test_that("mlogLoss different classes", {

  Levs = 8
  Size = 100
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)

  # no warnings
  expect_silent(res1 <- mlogLoss(y, xm))
  # estimated
  expect_true(!is.nan(res1))

  expect_silent(res2 <- mlogLoss(y, as.data.frame(xm)))
  expect_true(res1 == res2)
  expect_true(all(res1 == res2))

  expect_silent(res3 <- mlogLoss(as.character(y), as.data.frame(xm)))
  expect_true(res1 == res3)
  expect_true(all(res1 == res3))

  expect_silent(res4 <- mlogLoss(as.factor(y), as.data.frame(xm)))
  expect_true(res1 == res4)
  expect_true(all(res1 == res4))

})



test_that("mauc", {

  Levs = 8
  Size = 100
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)
  # no warnings
  expect_silent(res1 <- mauc(y, xm))
  # estimated
  expect_true(!is.nan(res1$mauc))

  expect_silent(res2 <- mauc(y, as.data.frame(xm)))
  expect_true(res1$mauc == res2$mauc)
  expect_true(all(res1$auc == res2$auc))

  expect_silent(res3 <- mauc(as.character(y), as.data.frame(xm)))
  expect_true(res1$mauc == res3$mauc)
  expect_true(all(res1$auc == res3$auc))

  expect_silent(res4 <- mauc(as.factor(y), as.data.frame(xm)))
  expect_true(res1$mauc == res4$mauc)
  expect_true(all(res1$auc == res4$auc))

})



test_that("logLoss estimates with 0s and 1s as values", {

  data(testDF)
  glmModel <- glm(y ~ ., data=testDF, family="binomial")
  Preds <- predict(glmModel, type = 'response')

  Preds[1] = 0
  Preds[2] = 1
  logLoss(testDF$y, Preds)

})
