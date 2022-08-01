new_wand <- function(model_obj, best_model_params,
                     smooth_specs, training_params,
                     training_results,
                     outcome_info, mode,
                     blueprint) {
  hardhat::new_model(
    model_obj = model_obj,
    best_model_params = best_model_params,
    smooth_specs = smooth_specs,
    training_params = training_params,
    training_results = training_results,
    outcome_info = outcome_info,
    mode = mode,
    blueprint = blueprint,
    class = "wand"
  )
}
