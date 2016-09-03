
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


  ## test taken from caret written by Max Kuhn
  eps <- 1e-15

  classes <- LETTERS[1:3]

  test_dat1 <- data.frame(obs  = c("A", "A", "A", "B", "B", "C"),
                          pred = c("A", "A", "A", "B", "B", "C"),
                          A = c(1, .80, .51, .1, .2, .3),
                          B = c(0, .05, .29, .8, .6, .3),
                          C = c(0, .15, .20, .1, .2, .4))

  expected1 <- log(1-eps) + log(.8) + log(.51) + log(.8) + log(.6) + log(.4)
  expected1 <- -expected1/nrow(test_dat1)
  result1 <- mlogLoss(test_dat1$obs, test_dat1[,3:5])

  # test_dat2 <- test_dat1
  # test_dat2$A[1] <- NA
  #
  # expected2 <-  log(.8) + log(.51) + log(.8) + log(.6) + log(.4)
  # expected2 <- c(logLoss = -expected2/sum(complete.cases(test_dat2)))
  # result2 <- mlogLoss(test_dat2$obs, test_dat2[,3:5])

  expect_equal(result1, expected1)
  # expect_equal(result2, expected2)
  # expect_equal(result3, expected3)

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
