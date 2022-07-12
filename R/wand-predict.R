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
#'
#' @param ... Not used, but required for extensibility.
#'
#' @return
#'
#' A tibble of predictions. The number of rows in the tibble is guaranteed
#' to be the same as the number of rows in `new_data`.
#'
#' @examples
#' train <- mtcars[1:20,]
#' test <- mtcars[21:32, -1]
#'
#' # Fit
#' mod <- wand(mpg ~ cyl + log(drat), train)
#'
#' # Predict, with preprocessing
#' predict(mod, test)
#'
#' @export
predict.wand <- function(object, new_data, type = "numeric", validation = T, ...) {
  forged <- hardhat::forge(new_data, object$blueprint)
  rlang::arg_match(type, valid_wand_predict_types())
  predict_wand_bridge(type, object, forged$predictors, validation)
}

valid_wand_predict_types <- function() {
  c("numeric")
}

# ------------------------------------------------------------------------------
# Bridge

predict_wand_bridge <- function(type, model, predictors, validation) {
  # predictors <- as.matrix(predictors)

  predict_function <- get_wand_predict_function(type)
  predictions <- predict_function(model, predictors, validation)

  hardhat::validate_prediction_size(predictions, predictors)

  predictions
}

get_wand_predict_function <- function(type) {
  switch(
    type,
    numeric = predict_wand_numeric
  )
}

# ------------------------------------------------------------------------------
# Implementation

# Again, from brulee
predict_wand_impl <- function(model, predictors, validation) {
  # Load model from raw object
  module <- hydrate_model(model$model_obj)
  # Use params from the best validation set if the validation flag is set here
  # and the model was trained with a validation set to begin with.
  if (validation & model$optimization_parameters$validation_prop > 0) {
    module_params <- lapply(model$validation_best_model_params, torch::torch_tensor)
  } else {
    module_params <- lapply(model$best_model_params, torch::torch_tensor)
  }
  # Load params into module and put into eval mode
  module$load_state_dict(module_params)
  module$eval()

  # Build dataset
  x <- build_wand_dataset(predictors,
                          smooth_features = model$smooth_features,
                          requires_grad = F)
  print(x$tensors)
  pred <- module(x)

  # replace torch nan's with NA
  pred[is.nan(pred)] <- NA
  # and return
  pred
}

predict_wand_numeric <- function(model, predictors, validation) {
  predictions <- predict_wand_impl(model, predictors, validation)
  hardhat::spruce_numeric(predictions)
}
