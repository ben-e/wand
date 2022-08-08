new_wand <- function(model_obj,
                     model_params,
                     outcome_info,
                     mode,
                     blueprint,
                     smooth_blueprints,
                     training_params,
                     smooth_specs,
                     training_results,
                     typical_df) {
  hardhat::new_model(
    # Needed to generate predictions from trained model
    model_obj = model_obj,
    model_params = model_params,
    outcome_info = outcome_info,
    mode = mode,
    # Needed to forge data before prediction
    blueprint = blueprint,
    smooth_blueprints = smooth_blueprints,
    # Model inspection: specification: used to plot model graph
    training_params = training_params,
    smooth_specs = smooth_specs,
    # Model inspection: training results, used to plot loss
    training_results = training_results,
    # Model inspection: training data info, used to plot smooths
    typical_df = typical_df,
    # hardhat meta
    class = "wand"
  )
}
