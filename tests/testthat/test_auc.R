
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

