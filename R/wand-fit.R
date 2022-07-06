# user Interface -----------------------------------------------------------------------------------

#' Fit a `wand`
#'
#' `wand()` fits a model.
#'
#' @param x Depending on the context:
#'
#'   * A __data frame__ of predictors.
#'   * A __matrix__ of predictors.
#'   * A __recipe__ specifying a set of preprocessing steps
#'     created from [recipes::recipe()].
#'
#' @param y When `x` is a __data frame__ or __matrix__, `y` is the outcome
#' specified as:
#'
#'   * A __data frame__ with 1 numeric column.
#'   * A __matrix__ with 1 numeric column.
#'   * A numeric __vector__.
#'
#' @param data When a __recipe__ or __formula__ is used, `data` is specified as:
#'
#'   * A __data frame__ containing both the predictors and the outcome.
#'
#' @param formula A formula specifying the outcome terms on the left-hand side,
#' and the predictor terms on the right-hand side.
#'
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `wand` object.
#'
#' @examples
#' predictors <- mtcars[, -1]
#' outcome <- mtcars[, 1]
#'
#' # XY interface
#' mod <- wand(predictors, outcome)
#'
#' # Formula interface
#' mod2 <- wand(mpg ~ ., mtcars)
#'
#' # Recipes interface
#' library(recipes)
#' rec <- recipe(mpg ~ ., mtcars)
#' rec <- step_log(rec, disp)
#' mod3 <- wand(rec, mtcars)
#'
#' @export
wand <- function(x, ...) {
  UseMethod("wand")
}

#' @export
#' @rdname wand
wand.default <- function(x, ...) {
  stop("`wand()` is not defined for a '", class(x)[1], "'.", call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname wand
wand.data.frame <- function(x, y, smooth_features, epochs = 10L, ...) {
  processed <- hardhat::mold(x, y)

  smooth_features <- names(dplyr::select(processed$predictors, {{ smooth_features }}))
  smooth_feature_indices <- which(names(processed$predictors) %in% smooth_features)

  wand_bridge(processed, smooth_feature_indices, epochs = epochs, ...)
}

# XY method - matrix

#' @export
#' @rdname wand
wand.matrix <- function(x, y, smooth_feature_indices, epochs = 10L, ...) {
  processed <- hardhat::mold(x, y)
  wand_bridge(processed, smooth_feature_indices, epochs = epochs, ...)
}

# Formula method

#' @export
#' @rdname wand
wand.formula <- function(formula, data, smooth_features, epochs = 10L, ...) {
  processed <- hardhat::mold(formula, data)

  # TODO Extract smooth features and their associated params from the formula
  smooth_features <- names(dplyr::select(processed$predictors, {{ smooth_features }}))
  smooth_feature_indices <- which(names(processed$predictors) %in% smooth_features)

  wand_bridge(processed, epochs = epochs, ...)
}

# Recipe method

#' @export
#' @rdname wand
wand.recipe <- function(x, data, smooth_features, epochs = 10L, ...) {
  processed <- hardhat::mold(x, data)

  smooth_features <- names(dplyr::select(processed$predictors, {{ smooth_features }}))
  smooth_feature_indices <- which(names(processed$predictors) %in% smooth_features)

  wand_bridge(processed, smooth_feature_indices, epochs = epochs, ...)
}

# Bridge -------------------------------------------------------------------------------------------

wand_bridge <- function(processed, smooth_feature_indices, epochs, ...) {
  # Checks
  if(!torch::torch_is_installed()) {
    rlang::abort("The torch backend has not been installed; use `torch::install_torch()`.")
  }

  if (is.numeric(epochs) & !is.integer(epochs)) {
    epochs <- as.integer(epochs)
  }

  # Get processed data
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # Fit modle
  fit <- wand_impl(predictors, outcome,
                   smooth_feature_indices,
                   epochs = epochs)

  # Construct wand object
  new_wand(
    model_obj = fit$model_obj,
    model_params_per_epoch = fit$model_params_per_epoch,
    loss = fit$loss,
    validation_loss = fit$validation_loss,
    best_epoch = fit$best_epoch,
    validation_best_epoch = fit$validation_best_epoch,
    smooth_parameters = fit$smooth_parameters,
    optimization_parameters = fit$optimization_parameters,
    blueprint = processed$blueprint
  )
}


# Implementation -----------------------------------------------------------------------------------
wand_impl <- function(x, y, smooth_feature_indices, epochs) {
  # set torch seed
  torch::torch_manual_seed(4242)

  # Load data into torch dataset, then a torch dataloader
  ds <- matrix_to_wand_dataset(as.matrix(x), as.matrix(y), smooth_feature_indices)
  dl <- torch::dataloader(ds, batch_size = 32, shuffle = T)

  # Initialize wand module
  n_smooth_features <- length(smooth_feature_indices)
  if (n_smooth_features > 0)
    hidden_units <- lapply(1:n_smooth_features, \(x) c(32, 32, 16))
  else
    hidden_units <- list()

  n_linear_features <- ncol(x) - n_smooth_features
  model <- wand_module(n_linear_features, hidden_units)

  # Initialize the optimizer
  # TODO optimizer should be an arg
  optimizer <- torch::optim_sgd(model$parameters, lr = 0.01)

  # Initialize vectors/lists that track training
  # TODO epochs should be an arg
  loss_min <- 10^38
  best_epoch <- 1L
  loss_vec <- rep(NA_real_, epochs)
  val_loss_min <- 10^38
  val_best_epoch <- 1L
  val_loss_vec <- rep(NA_real_, epochs)
  model_params_per_epoch <- list()

  # Run training loop
  for (epoch in 1:epochs) {
    # actual training - iterate over batches
    coro::loop(
      for (batch in dl) {
        pred <- model(batch$x_linear, batch$x_smooth)
        # TODO loss fn should be an arg, as should class weights
        loss <- torch::nnf_mse_loss(pred$flatten(), batch$y$flatten())

        optimizer$zero_grad()
        loss$backward()
        optimizer$step()
      }
    )

    # calculate whole training set loss and validation loss
    # TODO validation loss
    pred <- model(dl$dataset$tensors$x_linear, dl$dataset$tensors$x_smooth)
    loss <- torch::nnf_mse_loss(pred$flatten(), dl$dataset$tensors$y$flatten())

    # save loss
    loss_current <- loss$item()
    loss_vec[epoch] <- loss_current

    if (loss_current < loss_min) {
      loss_min <- loss_current
      best_epoch <- epoch
    }

    # TODO validation
    # if (val_loss_current < val_loss_min) {
    #   val_loss_min <- val_loss_current
    #   val_best_epoch <- epoch
    # }

    # TODO Save model params

    # Update user
    # TODO validation loss
    msg <- paste0("epoch: ", epoch, " loss: ", signif(loss_current, 5))
    rlang::inform(msg)
  }

  # Return model
  list(
    model_obj = list(),
    model_params_per_epoch = list(),
    loss = loss_vec,
    validation_loss = val_loss_vec,
    best_epoch = best_epoch,
    validation_best_epoch = val_best_epoch,
    smooth_parameters = list(),
    optimization_parameters = list(epochs = epochs)
  )
}
