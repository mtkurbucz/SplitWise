#' @title Transform Features (Univariate Logic)
#' @description
#'   Given the decisions (dummy or linear) for each predictor, produce a transformed
#'   data frame. Dummy columns are 0/1 based on the cutoff.
#'
#' @param X Original predictor data frame.
#' @param decisions The list returned by \code{decide_variable_type_univariate}.
#'
#' @return A new data frame with either the original column or a dummy column for each variable.
#' @keywords internal
#'
transform_features_univariate <- function(X, decisions) {

  transformed <- X[, FALSE]

  for (col_name in names(decisions)) {
    decision <- decisions[[col_name]]

    # Always keep the original numeric column
    transformed[[col_name]] <- X[[col_name]]

    if (decision$type == "dummy") {
      # Create a single 'range' dummy from the min & max of cutoffs
      cutoffs <- decision$cutoffs
      if (!is.null(cutoffs) && length(cutoffs) >= 1) {
        d_col <- as.numeric(
          X[[col_name]] >= min(cutoffs) &
            X[[col_name]] <= max(cutoffs)
        )
        dummy_name <- paste0(col_name, "_dummy")
        transformed[[dummy_name]] <- d_col
      }
    }
  }

  return(transformed)
}
