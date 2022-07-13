new_wand <- function(model_obj, smooth_specs, best_model_params, training_params, blueprint) {
  hardhat::new_model(
    model_obj = model_obj,
    smooth_specs = smooth_specs,
    best_model_params = best_model_params,
    training_params = training_params,
    blueprint = blueprint,
    class = "wand"
  )
}
