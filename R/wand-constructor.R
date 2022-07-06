new_wand <- function(model_obj, model_params_per_epoch, loss, validation_loss,
                     best_epoch, validation_best_epoch, smooth_features,
                     optimization_parameters, blueprint) {
  # if (!inherits(model_obj, "raw")) {
  #   rlang::abort("'model_obj' should be a raw vector.")
  # }
  if (!is.list(model_params_per_epoch)) {
    rlang::abort("'model_params_per_epoch' should be a list")
  }
  if (!is.vector(loss) || !is.numeric(loss)) {
    rlang::abort("'loss' should be a numeric vector")
  }
  if (!is.vector(validation_loss) || !is.numeric(validation_loss)) {
    rlang::abort("'validation_loss' should be a numeric vector")
  }
  if (!is.vector(best_epoch) || !is.integer(best_epoch)) {
    rlang::abort("'best_epoch' should be an integer")
  }
  if (!is.vector(validation_best_epoch) || !is.integer(validation_best_epoch)) {
    rlang::abort("'validation_best_epoch' should be an integer")
  }
  if (!is.list(smooth_features)) {
    rlang::abort("'smooth_features' should be a list")
  }
  if (!is.list(optimization_parameters)) {
    rlang::abort("'optimization_parameters' should be a list")
  }
  if (!inherits(blueprint, "hardhat_blueprint")) {
    rlang::abort("'blueprint' should be a hardhat blueprint")
  }

  hardhat::new_model(
    model_obj = model_obj,
    model_params_per_epoch = model_params_per_epoch,
    loss = loss,
    validation_loss = validation_loss,
    best_epoch = best_epoch,
    validation_best_epoch = validation_best_epoch,
    smooth_features = smooth_features,
    optimization_parameters = optimization_parameters,
    blueprint = blueprint,
    class = "wand"
  )
}
