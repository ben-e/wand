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
wand.data.frame <- function(x, y,
                            smooth_features,
                            # batch
                            batch_size = 32L,
                            validation_prop = 0.1,
                            # training
                            epochs = 10L,
                            learn_rate = 0.01,
                            momentum = 0,
                            dampening = 0,
                            weight_decay = 0,
                            nesterov = F,
                            stop_iter = 5L,
                            stop_iter_validation = 5L,
                            # misc
                            gpu = F,
                            ...) {
  processed <- hardhat::mold(x, y)
  wand_bridge(processed,
              smooth_features,
              batch_size = batch_size,
              validation_prop = validation_prop,
              epochs = epochs,
              learn_rate = learn_rate,
              momentum = momentum,
              dampening = dampening,
              weight_decay = weight_decay,
              nesterov = nesterov,
              stop_iter = stop_iter,
              stop_iter_validation = stop_iter_validation,
              gpu = gpu,
              ...)
}

# XY method - matrix

#' @export
#' @rdname wand
wand.matrix <- function(x, y,
                        smooth_features,
                        # batch
                        batch_size = 32L,
                        validation_prop = 0.1,
                        # training
                        epochs = 10L,
                        learn_rate = 0.01,
                        momentum = 0,
                        dampening = 0,
                        weight_decay = 0,
                        nesterov = F,
                        stop_iter = 5L,
                        stop_iter_validation = 5L,
                        # misc
                        gpu = F,
                        ...) {
  processed <- hardhat::mold(x, y)
  wand_bridge(processed,
              smooth_features,
              batch_size = batch_size,
              validation_prop = validation_prop,
              epochs = epochs,
              learn_rate = learn_rate,
              momentum = momentum,
              dampening = dampening,
              weight_decay = weight_decay,
              nesterov = nesterov,
              stop_iter = stop_iter,
              stop_iter_validation = stop_iter_validation,
              gpu = gpu,
              ...)
}

# Formula method

#' @export
#' @rdname wand
wand.formula <- function(formula,
                         data,
                         # batch
                         batch_size = 32L,
                         validation_prop = 0.1,
                         # training
                         epochs = 10L,
                         learn_rate = 0.01,
                         momentum = 0,
                         dampening = 0,
                         weight_decay = 0,
                         nesterov = F,
                         stop_iter = 5L,
                         stop_iter_validation = 5L,
                         # misc
                         gpu = F,
                         ...) {
  # Get smooth features form the formula
  smooth_features <- list()
  for (term in attr(terms(formula), "term.labels")) {
    # TODO I don't like that this assumes/requires that smoothers start with s_, perhaps
    # use a registry of smoothers?
    if (grepl("^s_", term)) {
      # save smoother
      smooth_features[[length(smooth_features) + 1]] <- eval(parse(text = term))
      # update formula
      # TODO I think this should be done using a blueprint, but I'm having trouble with those.
      formula <- update(
        formula,
        paste0(". ~ . - ", term,
               " + ",  paste0(sapply(smooth_features[[length(smooth_features)]]$features, quo_name),
                              collapse = " + "))
      )
    }
  }

  processed <- hardhat::mold(formula, data)
  wand_bridge(processed,
              smooth_features,
              batch_size = batch_size,
              validation_prop = validation_prop,
              epochs = epochs,
              learn_rate = learn_rate,
              momentum = momentum,
              dampening = dampening,
              weight_decay = weight_decay,
              nesterov = nesterov,
              stop_iter = stop_iter,
              stop_iter_validation = stop_iter_validation,
              gpu = gpu,
              ...)
}

# Recipe method

#' @export
#' @rdname wand
wand.recipe <- function(x,
                        data,
                        smooth_features,
                        # batch
                        batch_size = 32L,
                        validation_prop = 0.1,
                        # training
                        epochs = 10L,
                        learn_rate = 0.01,
                        momentum = 0,
                        dampening = 0,
                        weight_decay = 0,
                        nesterov = F,
                        stop_iter = 5L,
                        stop_iter_validation = 5L,
                        # misc
                        gpu = F,
                        ...) {
  processed <- hardhat::mold(x, data)
  wand_bridge(processed,
              smooth_features,
              batch_size = batch_size,
              validation_prop = validation_prop,
              epochs = epochs,
              learn_rate = learn_rate,
              momentum = momentum,
              dampening = dampening,
              weight_decay = weight_decay,
              nesterov = nesterov,
              stop_iter = stop_iter,
              stop_iter_validation = stop_iter_validation,
              gpu = gpu,
              ...)
}

# Bridge -------------------------------------------------------------------------------------------

wand_bridge <- function(processed,
                        smooth_features,
                        # batch
                        batch_size = 32L,
                        validation_prop = 0.1,
                        # training
                        epochs = 10L,
                        learn_rate = 0.01,
                        momentum = 0,
                        dampening = 0,
                        weight_decay = 0,
                        nesterov = F,
                        stop_iter = 5L,
                        stop_iter_validation = 5L,
                        # misc
                        gpu = F,
                        ...) {
  # Checks
  if(!torch::torch_is_installed()) {
    rlang::abort("The torch backend has not been installed; use `torch::install_torch()`.")
  }
  if (is.numeric(epochs) & !is.integer(epochs)) {
    epochs <- as.integer(epochs)
  }

  # Make sure smooth features are named
  if (!missing(smooth_features) &&
      (is.null(names(smooth_features)) & length(smooth_features) > 0)) {
    names(smooth_features) <- paste0("smooth_", 1:length(smooth_features))
  }

  # Get processed data
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  # Fit model
  fit <- wand_impl(predictors, outcome,
                   smooth_features,
                   batch_size = batch_size,
                   validation_prop = validation_prop,
                   epochs = epochs,
                   learn_rate = learn_rate,
                   momentum = momentum,
                   dampening = dampening,
                   weight_decay = weight_decay,
                   nesterov = nesterov,
                   stop_iter = stop_iter,
                   stop_iter_validation = stop_iter_validation,
                   gpu = gpu)

  # Construct wand object
  new_wand(
    model_obj = fit$model_obj,
    model_params_per_epoch = fit$model_params_per_epoch,
    loss = fit$loss,
    validation_loss = fit$validation_loss,
    best_epoch = fit$best_epoch,
    validation_best_epoch = fit$validation_best_epoch,
    smooth_features = fit$smooth_features,
    optimization_parameters = fit$optimization_parameters,
    blueprint = processed$blueprint
  )
}


# Implementation -----------------------------------------------------------------------------------
wand_impl <- function(x, y,
                      smooth_features,
                      batch_size,
                      validation_prop,
                      epochs,
                      learn_rate,
                      momentum,
                      dampening,
                      weight_decay,
                      nesterov,
                      stop_iter,
                      stop_iter_validation,
                      gpu) {
  # set torch seed
  torch::torch_manual_seed(4242)

  # check args
  if (gpu)
    rlang::warn("GPU support is not currently implemented.")

  # Load data into torch dataset, then a torch dataloader
  # including validation data
  # straight from brulee, thank you :)
  validation_prop <- 0.1
  if (validation_prop > 0) {
    in_val <- sample(seq_along(y), floor(length(y) * validation_prop))
    x_val <- x[in_val, , drop = FALSE]
    y_val <- y[in_val]
    x <- x[-in_val, , drop = FALSE]
    y <- y[-in_val]

    ds_val <- build_wand_dataset(x_val, y_val, smooth_features, requires_grad = F)
  }
  ds <- build_wand_dataset(x, y, smooth_features)
  dl <- torch::dataloader(ds, batch_size = batch_size)

  # Initialize wand modules
  model <- wand_module(ncol(ds$tensors$x_linear), smooth_features)

  # Initialize the optimizer
  # TODO users should be able to pass in an optimizer and corresponding arguments
  optimizer <- torch::optim_sgd(model$parameters,
                                lr = learn_rate,
                                momentum = momentum,
                                dampening = dampening,
                                weight_decay = weight_decay,
                                nesterov = nesterov)

  # Initialize vectors/lists that track training
  # TODO epochs should be an arg
  loss_min <- 10^38
  best_epoch <- 1L
  loss_vec <- rep(NA_real_, epochs)
  consec_iters_without_improvement <- 0
  val_loss_min <- 10^38
  val_best_epoch <- 1L
  val_loss_vec <- rep(NA_real_, epochs)
  consec_iters_without_val_improvement <- 0
  model_params_per_epoch <- list()
  signif_digits <- 4

  # Run training loop
  for (epoch in 1:epochs) {
    # actual training - iterate over batches
    coro::loop(
      for (batch in dl) {
        pred <- model(batch)
        # TODO loss fn should be an arg, as should class weights
        loss <- torch::nnf_mse_loss(pred, batch$y)

        optimizer$zero_grad()
        loss$backward()
        optimizer$step()
      }
    )

    # calculate whole training set loss and validation loss
    pred <- model(ds$tensors)
    loss <- torch::nnf_mse_loss(pred, ds$tensors$y)
    loss_current <- loss$item()
    loss_vec[epoch] <- loss_current

    if (validation_prop > 0) {
      pred_val <- model(ds_val$tensors)
      loss_val <- torch::nnf_mse_loss(pred_val, ds_val$tensors$y)
      val_loss_current <- loss_val$item()
      val_loss_vec[epoch] <- val_loss_current
    }

    if (signif(loss_current, signif_digits) < loss_min) {
      loss_min <- signif(loss_current, signif_digits)
      best_epoch <- epoch
      consec_iters_without_improvement <- 0
    } else {
      consec_iters_without_improvement <- consec_iters_without_improvement + 1
    }

    if (signif(val_loss_current, 4) < val_loss_min) {
      val_loss_min <- signif(val_loss_current, 4)
      val_best_epoch <- val_best_epoch
      consec_iters_without_val_improvement <- 0
    } else {
      consec_iters_without_val_improvement <- consec_iters_without_val_improvement + 1
    }

    # Update user
    msg <- paste0("epoch: ", epoch,
                  " loss: ", signif(loss_current, signif_digits),
                  " val_loss: ", signif(val_loss_current, signif_digits))
    rlang::inform(msg)
  }

  # Break if no improvement
  if (consec_iters_without_improvement >= stop_iter ||
      consec_iters_without_val_improvement >= stop_iter_validation)
    break()

  # Return model
  list(
    model_obj = list(),
    model_params_per_epoch = list(),
    loss = loss_vec,
    validation_loss = val_loss_vec,
    best_epoch = best_epoch,
    validation_best_epoch = val_best_epoch,
    smooth_features = list(),
    optimization_parameters = list(epochs = epochs,
                                   learn_rate = learn_rate,
                                   batch_size = batch_size)
  )
}
