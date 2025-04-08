test_that("splitwise function works correctly with default parameters", {
  # Example dataset
  data <- mtcars
  formula <- mpg ~ .

  # Run the splitwise function
  model <- splitwise(formula, data)

  # Check that the model is of the correct class
  expect_s3_class(model, "splitwise_lm")

  # Check that the model has coefficients
  expect_true(length(coef(model)) > 0)
})

test_that("splitwise function handles different transformation modes", {
  data <- mtcars
  formula <- mpg ~ .

  # Test univariate transformation mode
  model_uni <- splitwise(formula, data, transformation_mode = "univariate")
  expect_s3_class(model_uni, "splitwise_lm")

  # Test iterative transformation mode
  model_iter <- splitwise(formula, data, transformation_mode = "iterative")
  expect_s3_class(model_iter, "splitwise_lm")
})

test_that("splitwise function respects stepwise direction parameter", {
  data <- mtcars
  formula <- mpg ~ .

  # Test forward selection
  model_forward <- splitwise(formula, data, direction = "forward")
  expect_s3_class(model_forward, "splitwise_lm")

  # Test backward elimination
  model_backward <- splitwise(formula, data, direction = "backward")
  expect_s3_class(model_backward, "splitwise_lm")

  # Test both directions
  model_both <- splitwise(formula, data, direction = "both")
  expect_s3_class(model_both, "splitwise_lm")
})
