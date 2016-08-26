
context("auc Tests")

test_that("auc binary error", {

  Levs = 8
  Size = 100
  y = sample(1:Levs, Size, replace = TRUE)

  xm = matrix(runif(Levs*Size), ncol = Levs)
  xm = xm/rowSums(xm)

  expect_error(auc(y, xm)
    , "auc only works for binary outcomes at this time")

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

