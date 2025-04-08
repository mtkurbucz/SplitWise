#' @title Decide Variable Type (Iterative)
#' @description
#'   A stepwise variable selection method that iteratively chooses each variable's best form
#'   (either "linear" or "dummy") based on model AIC improvement. Supports forward, backward,
#'   or both-direction stepwise logic.
#'
#'   Dummy forms are created using a shallow (maxdepth = 2) rpart tree fit to the partial
#'   residuals from the current model. The best split is converted into a binary dummy.
#'
#' @param X A data frame of predictors (no response).
#' @param Y A numeric vector (the response).
#' @param minsplit Minimum number of observations in a node to consider splitting. Default 5.
#' @param direction Stepwise strategy: "forward", "backward", or "both". Default is "backward".
#' @param ... Additional arguments (currently unused).
#'
#' @return A named list of decisions, where each element contains:
#'   \item{type}{Either "dummy" or "linear".}
#'   \item{cutoff}{The split value (if type == "dummy").}
#' @importFrom stats lm AIC coef
#' @importFrom rpart rpart rpart.control
#' @keywords internal
#'
decide_variable_type_iterative <- function(X, Y,
                                           minsplit = 5,
                                           direction = c("backward","forward","both"),
                                           ...) {
  direction <- match.arg(direction)

  #--------------------------------------------------------------------------
  # 1) HELPER FUNCTIONS
  #--------------------------------------------------------------------------

  df_design <- data.frame(Y = Y)
  decisions <- list()

  add_var_to_design <- function(df, Xcol, var_name, type, cutoff = NULL) {
    df_new <- df
    if (type == "linear") {
      df_new[[var_name]] <- Xcol
    } else {
      dummy_name <- paste0(var_name, "_dummy")
      df_new[[dummy_name]] <- as.numeric(Xcol >= cutoff)
    }
    df_new
  }

  remove_var_from_design <- function(df, var_name, type) {
    df_new <- df
    if (type == "linear") {
      df_new[[var_name]] <- NULL
    } else {
      dummy_name <- paste0(var_name, "_dummy")
      df_new[[dummy_name]] <- NULL
    }
    df_new
  }

  get_partial_resid <- function(mod, x) {
    coefs <- coef(mod)
    term_name <- names(x)[1]
    if (term_name %in% names(coefs)) {
      mod$residuals + coefs[[term_name]] * x[[1]]
    } else {
      mod$residuals
    }
  }

  # The fix: use a data frame for rpart
  find_best_form <- function(mod_current, df_current, Xcol, var_name, minsplit) {
    # Evaluate linear
    df_lin <- add_var_to_design(df_current, Xcol, var_name, "linear")
    mod_lin <- lm(Y ~ ., data = df_lin)
    lin_aic <- AIC(mod_lin)

    # Evaluate dummy with partial residual
    part_resid <- get_partial_resid(mod_current, data.frame(x_col = Xcol))
    tree_df <- data.frame(y = part_resid, x = Xcol)

    tmp_tree <- rpart::rpart(
      formula = y ~ x,
      data = tree_df,
      control = rpart::rpart.control(maxdepth = 2, minsplit = minsplit)
    )

    dummy_aic <- Inf
    chosen_cut <- NULL
    if (!is.null(tmp_tree$splits) && nrow(tmp_tree$splits) >= 1) {
      single_cut <- tmp_tree$splits[1, "index"]  # best single split
      df_dummy <- add_var_to_design(df_current, Xcol, var_name, "dummy", cutoff = single_cut)
      mod_dummy <- lm(Y ~ ., data = df_dummy)
      dummy_aic <- AIC(mod_dummy)
      chosen_cut <- single_cut
    }

    if (lin_aic <= dummy_aic) {
      list(aic = lin_aic, type = "linear", cutoff = NULL)
    } else {
      list(aic = dummy_aic, type = "dummy", cutoff = chosen_cut)
    }
  }

  #--------------------------------------------------------------------------
  # 2) INITIAL SETUP
  #--------------------------------------------------------------------------
  if (direction == "backward") {
    # Start "full": pick best univariate form for each var
    for (var_name in names(X)) {
      # For "univariate" partial residual, just intercept-only:
      univariate_mod <- lm(Y ~ 1, data = df_design)
      best_form <- find_best_form(
        mod_current = univariate_mod,
        df_current = df_design,
        Xcol = X[[var_name]],
        var_name = var_name,
        minsplit = minsplit
      )
      decisions[[var_name]] <- list(type = best_form$type, cutoff = best_form$cutoff)
      df_design <- add_var_to_design(df_design, X[[var_name]], var_name,
                                     best_form$type, best_form$cutoff)
    }
  }
  mod_current <- lm(Y ~ ., data = df_design)
  best_aic <- AIC(mod_current)
  unused_vars <- setdiff(names(X), names(decisions))

  #--------------------------------------------------------------------------
  # 3) MAIN LOOP
  #--------------------------------------------------------------------------
  improvement <- TRUE
  while (improvement) {
    improvement <- FALSE
    best_var_choice <- NULL
    best_type <- NULL
    best_cutoff <- NULL
    best_new_aic <- best_aic
    best_switch_from <- NULL

    #---------------------------------------------
    # Forward or both => try adding
    #---------------------------------------------
    if (direction %in% c("forward", "both")) {
      if (length(unused_vars) > 0) {
        for (var_name in unused_vars) {
          x_col <- X[[var_name]]
          form_info <- find_best_form(mod_current, df_design, x_col, var_name, minsplit)
          if (form_info$aic < best_new_aic) {
            best_new_aic <- form_info$aic
            best_var_choice <- var_name
            best_type <- form_info$type
            best_cutoff <- form_info$cutoff
            best_switch_from <- NULL
          }
        }
      }
    }

    #---------------------------------------------
    # Backward or both => try removing or switching existing
    #---------------------------------------------
    if (direction %in% c("backward", "both")) {
      vars_in_model <- names(decisions)

      for (var_name in vars_in_model) {
        old_type <- decisions[[var_name]]$type

        # Remove
        df_drop <- remove_var_from_design(df_design, var_name, old_type)
        mod_drop <- lm(Y ~ ., data = df_drop)
        drop_aic <- AIC(mod_drop)
        if (drop_aic < best_new_aic) {
          best_new_aic <- drop_aic
          best_var_choice <- var_name
          best_type <- "remove"
          best_cutoff <- NULL
          best_switch_from <- old_type
        }

        # Switch forms
        df_temp <- df_drop
        mod_temp <- mod_drop
        form_info <- find_best_form(mod_temp, df_temp, X[[var_name]], var_name, minsplit)

        # If it improves AIC, we consider it
        if (form_info$aic < best_new_aic) {
          best_new_aic <- form_info$aic
          best_var_choice <- var_name
          best_type <- form_info$type
          best_cutoff <- form_info$cutoff
          best_switch_from <- old_type
        }
      }
    }

    #---------------------------------------------
    # Both or forward => also re-check switching existing variables
    # (similar to your original approach)
    #---------------------------------------------
    if (direction %in% c("forward", "both")) {
      for (var_name in names(decisions)) {
        current_type <- decisions[[var_name]]$type
        x_col <- X[[var_name]]
        df_temp <- remove_var_from_design(df_design, var_name, current_type)
        mod_temp <- lm(Y ~ ., data = df_temp)
        form_info <- find_best_form(mod_temp, df_temp, x_col, var_name, minsplit)

        if (form_info$aic < best_new_aic) {
          best_new_aic <- form_info$aic
          best_var_choice <- var_name
          best_type <- form_info$type
          best_cutoff <- form_info$cutoff
          best_switch_from <- current_type
        }
      }
    }

    #---------------------------------------------
    # 3D) Implement best improvement
    #---------------------------------------------
    if (!is.null(best_var_choice) && best_new_aic < best_aic) {
      improvement <- TRUE
      if (!is.null(best_switch_from) && best_switch_from != "remove" && best_type == "remove") {
        # We are removing a variable
        df_design <- remove_var_from_design(df_design, best_var_choice, best_switch_from)
        decisions[[best_var_choice]] <- NULL
        unused_vars <- union(unused_vars, best_var_choice)
      } else if (!is.null(best_switch_from) && best_switch_from %in% c("linear","dummy")) {
        # We are switching forms
        df_design <- remove_var_from_design(df_design, best_var_choice, best_switch_from)
        df_design <- add_var_to_design(df_design, X[[best_var_choice]], best_var_choice,
                                       best_type, best_cutoff)
        decisions[[best_var_choice]] <- list(type = best_type, cutoff = best_cutoff)
      } else {
        # We are adding a brand-new variable
        df_design <- add_var_to_design(df_design, X[[best_var_choice]], best_var_choice,
                                       best_type, best_cutoff)
        decisions[[best_var_choice]] <- list(type = best_type, cutoff = best_cutoff)
        unused_vars <- setdiff(unused_vars, best_var_choice)
      }
      # Update model & AIC
      mod_current <- lm(Y ~ ., data = df_design)
      best_aic <- AIC(mod_current)
    }
  } # end while

  decisions
}
