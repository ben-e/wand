new_wand <- function(model_obj,
                     best_model_params,
                     training_params,
                     training_results,
                     outcome_info,
                     predictor_info,
                     mode,
                     blueprint,
                     smooth_blueprints) {
  hardhat::new_model(
    model_obj = model_obj,
    best_model_params = best_model_params,
    training_params = training_params,
    training_results = training_results,
    outcome_info = outcome_info,
    predictor_info = predictor_info,
    mode = mode,
    blueprint = blueprint,
    smooth_blueprints = smooth_blueprints,
    class = "wand"
  )
}
