#' Predict from a `wand`
#'
#' @param object A `wand` object.
#'
#' @param new_data A data frame or matrix of new predictors.
#'
#' @param type A single character. The type of predictions to generate.
#' Valid options are:
#'
#' - `"numeric"` for numeric predictions.
#' - `"class"` for hard class predictions.
#' - `"prob"` for soft class probability predictions.
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   # Fit
#'   mod <- wand(mpg ~ cyl + log(drat) + s_mlp(hp), mtcars)
#'
#'   # Predict, with preprocessing
#'   predict(mod, mtcars)
#' }
#' }
#'
#' @export
predict.wand <- function(object, new_data, type = NULL, ...) {
  forged_linear <- hardhat::forge(new_data, object$blueprint)$predictors

  # recall that recipes are molded/forged on downstream
  if ("recipe_blueprint" %in% class(object$blueprint)) {
    forged_smooths <- lapply(object$smooth_blueprints,
                             \(spec) hardhat::forge(forged_linear, spec)$predictors)
  } else {
    forged_smooths <- lapply(object$smooth_blueprints,
                             \(spec) hardhat::forge(new_data, spec)$predictors)
  }

  # Remove smoothed terms from the linear data
  forged_linear <- dplyr::select(forged_linear, dplyr::all_of(object$predictor_info$linear_predictors))

  if (is.null(type)) {
    if (object$mode == "regression")
      type <- "numeric"
    else if (object$mode == "classification")
      type <- "class"
  }

  rlang::arg_match(type, valid_wand_predict_types())
  predict_wand_bridge(type, object, forged_linear, forged_smooths)
}

valid_wand_predict_types <- function() {
  c("numeric", "class", "prob")
}

# --------------------------------------------------------------------------------------------------
# Bridge

predict_wand_bridge <- function(type, object, linear_predictors, smooth_predictors) {
  # Load data into a wand friendly dataset, no outcome or gradient required
  wand_predictors <- build_wand_dataset(linear_predictors, smooth_predictors,
                                        requires_grad = F)

  # Rehydrate the model
  object$model_obj <- hydrate_model(object$model_obj)

  # Load model params into model and set eval mode
  object$model_obj$load_state_dict(lapply(object$model_params, torch::torch_tensor))
  object$model_obj$eval()

  predict_function <- get_wand_predict_function(type)
  predictions <- predict_function(object$model_obj, wand_predictors, object$outcome_info)
  hardhat::validate_prediction_size(predictions,
                                    dplyr::bind_cols(linear_predictors, smooth_predictors))

  predictions
}

get_wand_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_wand_numeric,
    prob = predict_wand_prob,
    class = predict_wand_class
  )
}

# --------------------------------------------------------------------------------------------------
# Implementation

predict_wand_numeric <- function(model, predictors, outcome_info) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)[ , 1]
  predictions <- predictions * outcome_info$sd + outcome_info$mean
  # convert NaN to NA
  predictions[is.nan(predictions)] <- NA
  # Return spruced predictions
  hardhat::spruce_numeric(predictions)
}

predict_wand_prob <- function(model, predictors, outcome_info) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)
  predictions[is.nan(predictions)] <- NA
  hardhat::spruce_prob(pred_levels = outcome_info$classes, predictions)
}

predict_wand_class <- function(model, predictors, outcome_info) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)
  predictions[is.nan(predictions)] <- NA
  predictions <- apply(predictions, 1,
                       \(x) if (any(is.na(x))) NA else which.max(x))
  hardhat::spruce_class(factor(outcome_info$classes[predictions], levels = outcome_info$classes))
}
