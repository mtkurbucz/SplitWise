test_that("Splitwise vs step() function with different transformation modes", {

  set.seed(123)
  data(mtcars)

  directions <- c("backward", "forward", "both")
  transformation_modes <- c("univariate", "iterative")

  # Fix: Swap column order: Splitwise_AIC first, then Step_AIC
  results <- data.frame(Direction = character(),
                        Transformation_Mode = character(),
                        Splitwise_AIC = numeric(),
                        Step_AIC = numeric())

  for (dir in directions) {
    # Fit the initial model
    initial_model <- lm(mpg ~ ., data = mtcars)

    # Apply the step() function
    step_model <- step(initial_model, direction = dir, trace = FALSE)
    step_aic <- AIC(step_model)

    for (mode in transformation_modes) {
      # Apply the splitwise function with the current transformation mode
      splitwise_model <- splitwise(mpg ~ ., data = mtcars,
                                   transformation_mode = mode,
                                   direction = dir, trace = FALSE)
      splitwise_aic <- AIC(splitwise_model)

      # Compare the AIC values
      expect_lte(splitwise_aic, step_aic,
                 label = paste("AIC comparison for", dir, "direction and", mode, "mode"))

      # Store the results (Fix: Swap column order)
      results <- rbind(results, data.frame(Direction = dir,
                                           Transformation_Mode = mode,
                                           Splitwise_AIC = splitwise_aic,
                                           Step_AIC = step_aic))
    }
  }

  # Format and print the comparison table
  cat("\n\n")
  cat(format("Direction", width = 12, justify = "left"),
      format("Transformation Mode", width = 20, justify = "left"),
      format("SplitWise AIC", width = 15, justify = "right"),
      format("Step AIC", width = 10, justify = "right"), "\n")
  cat(rep("-", 60), sep = "", "\n")
  for (i in 1:nrow(results)) {
    cat(format(results$Direction[i], width = 12, justify = "left"),
        format(results$Transformation_Mode[i], width = 20, justify = "left"),
        format(sprintf("%.4f", results$Splitwise_AIC[i]), width = 15, justify = "right"),
        format(sprintf("%.4f", results$Step_AIC[i]), width = 10, justify = "right"), "\n")
  }
  cat("\n")
})
