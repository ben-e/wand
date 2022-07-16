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
#'   train <- mtcars[1:20,]
#'   test <- mtcars[21:32, -1]
#'
#'   # Fit
#'   mod <- wand(mpg ~ cyl + log(drat), train)
#'
#'   # Predict, with preprocessing
#'   predict(mod, test)
#' }
#' }
#'
#' @export
predict.wand <- function(object, new_data, type = NULL, ...) {
  forged <- hardhat::forge(new_data, object$blueprint)

  if (is.null(type)) {
    if (object$mode == "regression")
      type <- "numeric"
    else if (object$mode == "classification")
      type <- "class"
  }

  rlang::arg_match(type, valid_wand_predict_types())
  predict_wand_bridge(type, object, forged$predictors)
}

valid_wand_predict_types <- function() {
  c("numeric", "class", "prob")
}

# --------------------------------------------------------------------------------------------------
# Bridge

predict_wand_bridge <- function(type, model, predictors) {
  # Load data into a wand friendly dataset, no outcome or gradient required
  wand_predictors <- build_wand_dataset(predictors,
                                        smooth_specs = model$smooth_specs,
                                        requires_grad = F)

  # Rehydrate the model
  model$model_obj <- hydrate_model(model$model_obj)

  # Load model params into model and set eval mode
  model$model_obj$load_state_dict(lapply(model$best_model_params, torch::torch_tensor))
  model$model_obj$eval()

  predict_function <- get_wand_predict_function(type)
  predictions <- predict_function(model$model_obj, wand_predictors, model$outcome_info$classes)

  hardhat::validate_prediction_size(predictions, predictors)

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

predict_wand_numeric <- function(model, predictors, levels = NULL) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)[ , 1]
  # convert NaN to NA
  predictions[is.nan(predictions)] <- NA
  # Return spruced predictions
  hardhat::spruce_numeric(predictions)
}

predict_wand_prob <- function(model, predictors, levels) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)
  predictions[is.nan(predictions)] <- NA
  hardhat::spruce_prob(pred_levels = levels, predictions)
}

predict_wand_class <- function(model, predictors, levels) {
  predictions <- model(predictors$tensors)
  predictions <- as.array(predictions)
  predictions[is.nan(predictions)] <- NA
  predictions <- apply(predictions, 1,
                       \(x) if (any(is.na(x))) NA else which.max(x))
  hardhat::spruce_class(factor(levels[predictions], levels = levels))
}
