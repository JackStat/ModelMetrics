
context("logLoss Tests")

test_that("mlogLoss character/factor actual", {

  Levs = 8
  Size = 1000000
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)

  m1 <- mlogLoss(y, xm)
  m2 <- mlogLoss(as.character(y), xm)
  m3 <- mlogLoss(as.factor(y), xm)

  expect_true(m1 == m2)
  expect_true(m1 == m3)

})
