new_wand <- function(model_obj,
                     model_params,
                     outcome_info,
                     predictor_info,
                     mode,
                     blueprint,
                     smooth_blueprints,
                     training_params,
                     smooth_specs,
                     training_results) {
  hardhat::new_model(
    # Needed to generate predictions from trained model
    model_obj = model_obj,
    model_params = model_params,
    outcome_info = outcome_info,
    # also model inspection: training data info, used to plot smooths
    predictor_info = predictor_info,
    mode = mode,
    # Needed to forge data before prediction
    blueprint = blueprint,
    smooth_blueprints = smooth_blueprints,
    # Model inspection: specification: used to plot model graph
    training_params = training_params,
    smooth_specs = smooth_specs,
    # Model inspection: training results, used to plot loss
    training_results = training_results,
    # hardhat meta
    class = "wand"
  )
}
