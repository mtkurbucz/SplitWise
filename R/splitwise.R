#' @title SplitWise Regression
#' @description
#' Transforms each numeric variable into either a single-split dummy or keeps it linear,
#' then runs \code{stats::step()} for stepwise selection. The user can choose a
#' simpler univariate transformation or an iterative approach.
#'
#' @param formula A formula specifying the response and (initial) predictors, e.g. \code{mpg ~ .}.
#' @param data A data frame containing the variables used in \code{formula}.
#' @param transformation_mode Either \code{"iterative"} or \code{"univariate"}. Default = \code{"iterative"}.
#' @param direction Stepwise direction: \code{"backward"}, \code{"forward"}, or \code{"both"}.
#' @param minsplit Minimum number of observations in a node to consider splitting. Default = 5.
#' @param criterion Either \code{"AIC"} or \code{"BIC"}. Default = \code{"AIC"}.
#'   \strong{Note}: If you choose \code{"BIC"}, you typically want \code{k = log(nrow(data))} in stepwise.
#' @param exclude_vars A character vector naming variables that should be forced to remain linear
#'   (i.e., no dummy splits allowed). Default = \code{NULL}.
#' @param verbose Logical; if \code{TRUE}, prints debug info in transformation steps. Default = \code{FALSE}.
#' @param trace If positive, \code{step()} prints info at each step. Default = 1.
#' @param steps Maximum number of steps for \code{step()}. Default = 1000.
#' @param k Penalty multiple for the number of degrees of freedom (used by \code{step()}).
#'   E.g. 2 for AIC, \code{log(n)} for BIC. Default = 2.
#' @param ... Additional arguments passed to \code{step()} or the iterative function.
#'
#' @return An S3 object of class \code{c("splitwise_lm", "lm")}, storing:
#' \item{splitwise_info}{List containing transformation decisions, final data, and call.}
#'
#' @examples
#' # Load the mtcars dataset
#' data(mtcars)
#'
#' # Univariate transformations (AIC-based, backward stepwise)
#' model_uni <- splitwise(
#'   mpg ~ .,
#'   data               = mtcars,
#'   transformation_mode = "univariate",
#'   direction           = "backward",
#'   trace               = 0
#' )
#' summary(model_uni)
#'
#' # Iterative approach (BIC-based, forward stepwise)
#' # Note: typically set k = log(nrow(mtcars)) for BIC in step().
#' model_iter <- splitwise(
#'   mpg ~ .,
#'   data               = mtcars,
#'   transformation_mode = "iterative",
#'   direction           = "forward",
#'   criterion           = "BIC",
#'   k                   = log(nrow(mtcars)),
#'   trace               = 0
#' )
#' summary(model_iter)
#'
#' @importFrom stats lm step model.frame setNames summary.lm as.formula AIC BIC
#' @export
#'
splitwise <- function(
    formula,
    data,
    transformation_mode = c("iterative", "univariate"),
    direction           = c("backward", "forward", "both"),
    minsplit            = 5,
    criterion           = c("AIC", "BIC"),
    exclude_vars        = NULL,
    verbose             = FALSE,
    trace               = 1,
    steps               = 1000,
    k                   = 2,
    ...
) {
  transformation_mode <- match.arg(transformation_mode)
  direction           <- match.arg(direction)
  criterion           <- match.arg(criterion)

  # 1) Extract model frame
  mf   <- stats::model.frame(formula, data)
  Y    <- mf[[1]]
  X    <- mf[-1]
  resp <- all.vars(formula)[1]

  # 2) Choose transformation approach
  if (transformation_mode == "univariate") {
    decisions <- decide_variable_type_univariate(
      X,
      Y,
      minsplit     = minsplit,
      criterion    = criterion,
      exclude_vars = exclude_vars,
      verbose      = verbose
    )
    X_trans <- transform_features_univariate(X, decisions)
  } else {
    # iterative approach
    decisions <- decide_variable_type_iterative(
      X,
      Y,
      minsplit     = minsplit,
      direction    = direction,
      criterion    = criterion,
      exclude_vars = exclude_vars,
      verbose      = verbose,
      ...
    )
    X_trans <- transform_features_iterative(X, decisions)
  }

  # 3) Combine with response
  df_final <- cbind(X_trans, setNames(list(Y), resp))

  # 4) Fit full model
  full_formula <- stats::as.formula(
    paste(resp, "~", paste(colnames(X_trans), collapse = " + "))
  )
  full_model <- stats::lm(full_formula, data = df_final)

  # 5) Stepwise model selection
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
  cat("SplitWise Linear Models\n")
  cat("Transformation mode:", x$splitwise_info$transformation_mode, "\n")
  cat("Call:\n")
  print(x$splitwise_info$call)

  # Grab all decisions
  decisions <- x$splitwise_info$decisions

  # Filter out the dummy ones
  dummy_vars <- names(Filter(function(d) d$type == "dummy", decisions))

  if (length(dummy_vars) > 0) {
    cat("\nDummy-Encoded Variables:\n")
    for (var_name in dummy_vars) {
      cut_vals <- decisions[[var_name]]$cutoff
      if (length(cut_vals) == 1) {
        # Single cutoff: 1 if x >= cutoff
        cat("  -", var_name,
            ": 1 if x >= ",
            format(round(cut_vals, 3), nsmall = 3),
            "; else 0\n"
        )
      } else if (length(cut_vals) == 2) {
        # Range dummy: 1 if lower < x < upper
        lower_val <- round(cut_vals[1], 3)
        upper_val <- round(cut_vals[2], 3)
        cat("  -", var_name,
            ": 1 if ", lower_val, " < x < ", upper_val,
            "; else 0\n"
        )
      } else {
        # fallback if something else is stored
        cat("  -", var_name, ": (unknown dummy definition?)\n")
      }
    }
  } else {
    cat("\nNo variables were dummy encoded.\n")
  }

  cat("\nCoefficients:\n")
  print(stats::coef(x))

  # Print both AIC & BIC for clarity
  cat("\nModel Fit Statistics:\n")
  cat("  AIC:", format(stats::AIC(x), digits = 5), "\n")
  cat("  BIC:", format(stats::BIC(x), digits = 5), "\n")

  invisible(x)
}

# ------------------------------------------------------------------------------

#' @title Summary Method for SplitWise Linear Model (Fancy)
#'
#' @param object A \code{"splitwise_lm"} object returned by \code{splitwise}.
#' @param ... Additional arguments passed to \code{summary.lm}.
#' @describeIn splitwise Provides a detailed summary, including how dummies were created.
#' @export
#'
summary.splitwise_lm <- function(object, ...) {

  # 1) Print the standard lm summary
  base_summary <- summary.lm(object, ...)
  print(base_summary)

  # 2) Grab transformations + produce custom info
  cat("Transformation Mode:", object$splitwise_info$transformation_mode, "\n")

  # Identify dummy variables
  decisions   <- object$splitwise_info$decisions
  dummy_vars  <- names(Filter(function(d) d$type == "dummy", decisions))

  if (length(dummy_vars) > 0) {
    cat("\nDummy-Encoded Variables:\n")
    for (var_name in dummy_vars) {
      cut_vals <- decisions[[var_name]]$cutoff

      if (length(cut_vals) == 1) {
        # Single cutoff: "1 if x >= cutoff"
        cat("  -", var_name,
            ": 1 if x >=",
            format(round(cut_vals, 3), nsmall = 3),
            "; else 0\n"
        )
      } else if (length(cut_vals) == 2) {
        # Range dummy: "1 if a < x < b"
        lower_val <- format(round(min(cut_vals), 3), nsmall = 3)
        upper_val <- format(round(max(cut_vals), 3), nsmall = 3)
        cat("  -", var_name,
            ": 1 if", lower_val, "< x <", upper_val,
            "; else 0\n"
        )
      } else {
        # Fallback if something else is stored
        cat("  -", var_name, ": (unknown dummy definition?)\n")
      }
    }
  } else {
    cat("\nNo variables were dummy encoded.\n")
  }

  # Print both AIC & BIC for clarity
  cat("\nFinal AIC:", format(stats::AIC(object), digits = 5), "\n")
  cat("Final BIC:", format(stats::BIC(object), digits = 5), "\n")

  invisible(base_summary)
}
