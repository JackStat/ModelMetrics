test_that("`confusionMatrix()` respects the `use_names` option", {
  act <- c(0, 0, 1)
  pred <- c(1, 0, 1)

  # Expect no names by default
  cm <- confusionMatrix(act, pred)
  expect_null(rownames(cm))
  expect_null(colnames(cm))

  cm2 <- confusionMatrix(act, pred, use_names = TRUE)
  expect_equal(rownames(cm2), c("predicted_0", "predicted_1"))
  expect_equal(colnames(cm2), c("actual_0", "actual_1"))
})
