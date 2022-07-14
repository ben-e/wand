new_wand <- function(model_obj, best_model_params,
                     loss, validation_loss,
                     smooth_specs, training_params,
                     outcome_info, mode,
                     blueprint) {
  hardhat::new_model(
    model_obj = model_obj,
    best_model_params = best_model_params,
    loss = loss,
    validation_loss = validation_loss,
    smooth_specs = smooth_specs,
    training_params = training_params,
    outcome_info = outcome_info,
    mode = mode,
    blueprint = blueprint,
    class = "wand"
  )
}
