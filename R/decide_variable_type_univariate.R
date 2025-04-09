#' @title Decide Variable Type (Univariate)
#' @description
#'   For each numeric predictor, fit a shallow (maxdepth=2) rpart tree against \code{Y}.
#'   If at least one split is found, mark it as "dummy", otherwise "linear".
#'   Used in the "simple" or "univariate" transformation mode.
#'
#' @param X A data frame of predictors (no response).
#' @param Y A numeric vector (the response).
#' @param minsplit Minimum number of observations in a node to consider splitting. Default 5.
#'
#' @return A named list of decisions, where each element contains:
#'   \item{type}{Either "dummy" or "linear".}
#'   \item{cutoff}{The split value (if type == "dummy").}
#'   \item{tree_model}{The fitted rpart model (for reference).}
#' @importFrom rpart rpart rpart.control
#' @keywords internal
#'
decide_variable_type_univariate <- function(X, Y, minsplit = 5) {

  decisions <- list()

  for (col_name in names(X)) {
    temp_df <- data.frame(x = X[[col_name]], y = Y)

    tree_model <- rpart::rpart(
      y ~ x,
      data = temp_df,
      model = TRUE,  # store data in the model
      control = rpart::rpart.control(maxdepth = 2, minsplit = minsplit)
    )

    if (!is.null(tree_model$splits) && nrow(tree_model$splits) >= 1) {
      split_points <- unique(tree_model$splits[, "index"])
      decisions[[col_name]] <- list(
        type     = "dummy",
        cutoffs  = split_points,
        tree_model = tree_model
      )
    } else {
      decisions[[col_name]] <- list(
        type     = "linear",
        cutoffs  = NULL,
        tree_model = tree_model
      )
    }
  }

  return(decisions)
}
