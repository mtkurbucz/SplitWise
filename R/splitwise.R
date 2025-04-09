#' @title SplitWise Regression
#' @description
#'   Transforms each numeric variable into either a single-split dummy or keeps it linear,
#'   then runs \code{stats::step()} to do stepwise selection.
#'   The user can choose a simpler univariate transformation or an iterative approach.
#'
#' @param formula A formula specifying the response and (initial) predictors, e.g. \code{mpg ~ .}.
#' @param data A data frame containing the variables used in \code{formula}.
#' @param transformation_mode Either \code{"univariate"} or \code{"iterative"}.
#' @param direction Stepwise direction: \code{"backward"}, \code{"forward"}, or \code{"both"}.
#' @param minsplit Control for the rpart trees (min # obs in node).
#' @param trace If positive, \code{step()} prints info at each step.
#' @param steps Maximum number of steps for \code{step()}.
#' @param k Penalty multiple for the number of degrees of freedom (e.g. 2 for AIC, log(n) for BIC).
#' @param ... Additional arguments passed to \code{step()}.
#'
#' @return An S3 object of class \code{c("splitwise_lm", "lm")}, storing:
#'   \item{splitwise_info}{List containing transformation decisions, final data, and call.}
#'
#' @examples
#' # Load the mtcars dataset
#' data(mtcars)
#'
#' # Univariate transformations
#' model_uni <- splitwise(
#'   mpg ~ .,
#'   data = mtcars,
#'   transformation_mode = "univariate",
#'   direction = "backward",
#'   trace = 0
#' )
#' summary(model_uni)
#' print(model_uni)
#'
#' # Iterative approach
#' model_iter <- splitwise(
#'   mpg ~ .,
#'   data = mtcars,
#'   transformation_mode = "iterative",
#'   direction = "forward",
#'   trace = 0
#' )
#' summary(model_iter)
#' print(model_iter)
#'
#' @importFrom stats lm step model.frame as.formula AIC
#' @export
#'
splitwise <- function(
    formula,
    data,
    transformation_mode = c("univariate", "iterative"),
    direction           = c("backward", "forward", "both"),
    minsplit            = 5,
    trace               = 1,
    steps               = 1000,
    k                   = 2,
    ...
) {
  transformation_mode <- match.arg(transformation_mode)
  direction           <- match.arg(direction)

  # 1) Extract model frame
  mf   <- stats::model.frame(formula, data)
  Y    <- mf[[1]]
  X    <- mf[-1]
  resp <- all.vars(formula)[1]

  # 2) Choose transformation approach
  if (transformation_mode == "univariate") {
    decisions <- decide_variable_type_univariate(X, Y, minsplit = minsplit)
    X_trans   <- transform_features_univariate(X, decisions)
  } else {
    # iterative approach (assume you already have these functions)
    decisions <- decide_variable_type_iterative(X, Y, minsplit = minsplit, direction = direction, ...)
    X_trans   <- transform_features_iterative(X, decisions)
  }

  # 3) Combine with response
  df_final <- cbind(X_trans, setNames(list(Y), resp))

  # 4) Fit full model
  full_formula <- stats::as.formula(
    paste(resp, "~", paste(colnames(X_trans), collapse = " + "))
  )
  full_model <- stats::lm(full_formula, data = df_final)

  # 5) Stepwise
  scope_list <- list(
    lower = stats::as.formula(paste(resp, "~ 1")),
    upper = full_formula
  )

  step_model <- stats::step(
    full_model,
    scope     = scope_list,
    direction = direction,
    trace     = trace,
    steps     = steps,
    k         = k,
    ...
  )

  # 6) Attach metadata
  step_model$splitwise_info <- list(
    transformation_mode = transformation_mode,
    decisions           = decisions,
    final_data          = df_final,
    call                = match.call()
  )

  class(step_model) <- c("splitwise_lm", class(step_model))
  return(step_model)
}

# ------------------------------------------------------------------------------

#' @title Print Method for SplitWise Linear Model
#'
#' @param x A \code{"splitwise_lm"} object returned by \code{splitwise}.
#' @param ... Additional arguments (unused).
#' @describeIn splitwise Prints a summary of the splitwise_lm object.
#' @export
#'
print.splitwise_lm <- function(x, ...) {
  cat("SplitWise Linear Model - Single-Split Dummy Encoding\n")
  cat("Transformation mode:", x$splitwise_info$transformation_mode, "\n")
  cat("Call:\n")
  print(x$splitwise_info$call)

  dummies <- names(Filter(function(d) d$type == "dummy", x$splitwise_info$decisions))
  if (length(dummies) > 0) {
    cat("\nVariables that were flagged as dummy:\n")
    cat(" ", paste(dummies, collapse = ", "), "\n")
  } else {
    cat("\nNo variables were dummy encoded.\n")
  }

  cat("\nCoefficients:\n")
  print(stats::coef(x))
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @title Summary Method for SplitWise Linear Model
#'
#' @param object A \code{"splitwise_lm"} object returned by \code{splitwise}.
#' @param ... Additional arguments passed to \code{summary.lm}.
#' @describeIn splitwise Provides a detailed summary of the splitwise_lm object.
#' @export
#'
summary.splitwise_lm <- function(object, ...) {

  # Compute the base summary of the lm object
  base_summary <- summary.lm(object, ...)

  # Print the standard summary
  print(base_summary)

  # Add your custom info
  cat("<< SplitWise Information >>\n")
  cat("Transformation Mode:", object$splitwise_info$transformation_mode, "\n")

  dummies <- names(Filter(function(d) d$type == "dummy", object$splitwise_info$decisions))
  if (length(dummies) > 0) {
    cat("Dummy-encoded variables:", paste(dummies, collapse = ", "), "\n")
  } else {
    cat("Dummy-encoded variables: None\n")
  }

  cat("Final AIC:", stats::AIC(object), "\n")

  invisible(base_summary)
}
