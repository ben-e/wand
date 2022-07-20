#' Fit a `wand` model
#'
#' `wand()` fits a wide and deep neural network, where the wide part of the network treats features
#' as linear, and the deep parts of the network uses neural network submodules to smooth features.
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
#' @param smooth_specs A named list of smooth specifications. A specification is simply the output
#'   oaf `s_` functions like `s_mlp`. If the list is not named, or if a __formula__ is used, the
#'   smooths will be named sequentially. When using a __formula__ the smooths are specified directly
#'   as part of the formula.
#' @param batch_size An integer giving the number of training samples included in each minibatch.
#' @param validation_prop The proportion of training samples assigned to the validation set.
#' @param epochs An integer giving the number of training epochs.
#' @param learn_rate The initial learning rate used by the optimizer.
#' @param stop_iter A non-negative integer giving the number of epochs with no improvement in loss
#'   before training is stopped. If `validation_prop > 0` then validation loss is used, otherwise
#'   loss is evaluated using the whole training set. Set this parameter to `Inf` to prevent any
#'   early stopping.
#' @param verbose A logical. When `TRUE` the loss, and validation loss if relevant, will be printed
#'   out for each epoch, along with other training messages.
#' @param ... Not currently used, but required for extensibility.
#'
#' @return
#'
#' A `wand` object.
#'
#' @details TODO :)
#'
#' @examples
#' \donttest{
#' if (torch::torch_is_installed()) {
#'   predictors <- mtcars[, -1]
#'   outcome <- mtcars[, 1]
#'
#'   # XY interface
#'   mod <- wand(predictors, outcome, smooth_specs = list(hp = s_mlp(hp)))
#'
#'   # Formula interface
#'   mod2 <- wand(mpg ~ s_mlp(hp) + disp, mtcars)
#'
#'   # Recipes interface
#'   library(recipes)
#'   rec <- recipe(mpg ~ ., mtcars)
#'   rec <- step_log(rec, disp)
#'   mod3 <- wand(rec, mtcars, smooth_specs = list(hp = s_mlp(hp)))
#' }
#' }
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
wand.data.frame <- function(x, y, smooth_specs,
                            # batch
                            batch_size = 32,
                            validation_prop = 0.1,
                            # training
                            epochs = 10,
                            learn_rate = 0.0003,
                            stop_iter = 5,
                            # misc
                            verbose = F,
                            ...) {
  processed <- hardhat::mold(x, y)

  wand_bridge(
    processed,
    smooth_specs,
    batch_size = batch_size,
    validation_prop = validation_prop,
    epochs = epochs,
    learn_rate = learn_rate,
    stop_iter = stop_iter,
    verbose = verbose,
    ...
  )
}

# XY method - matrix

#' @export
#' @rdname wand
wand.matrix <- function(x, y, smooth_specs,
                        # batch
                        batch_size = 32,
                        validation_prop = 0.1,
                        # training
                        epochs = 10,
                        learn_rate = 0.0003,
                        stop_iter = 5,
                        # misc
                        verbose = F,
                        ...) {
  processed <- hardhat::mold(x, y)
  wand_bridge(
    processed,
    smooth_specs,
    batch_size = batch_size,
    validation_prop = validation_prop,
    epochs = epochs,
    learn_rate = learn_rate,
    stop_iter = stop_iter,
    verbose = verbose,
    ...
  )
}

# Formula method

#' @export
#' @rdname wand
wand.formula <- function(formula, data,
                         # batch
                         batch_size = 32,
                         validation_prop = 0.1,
                         # training
                         epochs = 10,
                         learn_rate = 0.0003,
                         stop_iter = 5,
                         # misc
                         verbose = F,
                         ...) {
  # Get smooth specs form the formula
  smooth_specs <- list()
  for (term in attr(stats::terms(formula, data = data), "term.labels")) {
    # TODO I don't like that this assumes/requires that smoothers start with s_, perhaps
    # use a registry of smoothers? See parsnip's model registry as an example
    if (grepl("^s_", term)) {
      # save smoother
      smooth_specs[[length(smooth_specs) + 1]] <- eval(parse(text = term))
      # update formula
      # TODO I think this should be done using a blueprint, but I'm having trouble with those.
      formula <- stats::update(
        formula,
        paste0(". ~ . - ", term,
               " + ",  paste0(sapply(smooth_specs[[length(smooth_specs)]]$features,
                                     rlang::quo_name),
                              collapse = " + "))
      )
    }
  }

  processed <- hardhat::mold(formula, data)
  wand_bridge(
    processed,
    smooth_specs = smooth_specs,
    batch_size = batch_size,
    validation_prop = validation_prop,
    epochs = epochs,
    learn_rate = learn_rate,
    stop_iter = stop_iter,
    verbose = verbose,
    ...
  )
}

# Recipe method

#' @export
#' @rdname wand
wand.recipe <- function(x, data,
                        smooth_specs,
                        # batch
                        batch_size = 32,
                        validation_prop = 0.1,
                        # training
                        epochs = 10,
                        learn_rate = 0.0003,
                        stop_iter = 5,
                        # misc
                        verbose = F,
                        ...) {
  processed <- hardhat::mold(x, data)
  wand_bridge(
    processed,
    smooth_specs,
    batch_size = batch_size,
    validation_prop = validation_prop,
    epochs = epochs,
    learn_rate = learn_rate,
    stop_iter = stop_iter,
    verbose = verbose,
    ...
  )
}

# --------------------------------------------------------------------------------------------------
# Bridge

wand_bridge <- function(processed, smooth_specs,
                        # batch
                        batch_size,
                        validation_prop,
                        # training
                        epochs,
                        learn_rate,
                        stop_iter,
                        # misc
                        verbose,
                        ...) {
  predictors <- processed$predictors
  outcome <- processed$outcomes[[1]]

  if (!rlang::is_missing(smooth_specs) && is.null(names(smooth_specs)) && length(smooth_specs) > 0)
    names(smooth_specs) <- paste0("smooth_", 1:length(smooth_specs))

  fit <- wand_impl(
    predictors, outcome,
    smooth_specs,
    # batch
    batch_size,
    validation_prop,
    # training
    epochs,
    learn_rate = learn_rate,
    stop_iter = stop_iter,
    # misc
    verbose = verbose
  )

  new_wand(
    model_obj = fit$model_obj,
    best_model_params = fit$best_model_params,
    loss = fit$loss,
    validation_loss = fit$validation_loss,
    smooth_specs = fit$smooth_specs,
    training_params = fit$training_params,
    outcome_info = fit$outcome_info,
    mode = fit$mode,
    blueprint = processed$blueprint
  )
}


# --------------------------------------------------------------------------------------------------
# Implementation

wand_impl <- function(predictors, outcome,
                      smooth_specs,
                      # batch
                      batch_size,
                      validation_prop,
                      # training
                      epochs,
                      learn_rate,
                      stop_iter,
                      # misc
                      verbose) {

  torch::torch_manual_seed(4242)

  # Are we doing classification or regression?
  # Set up loss, dataset to match the mode
  if (is.factor(outcome)) {
    mode <- "classification"

    outcome_classes <- levels(outcome)
    n_classes <- length(outcome_classes)

    outcome_info <- list(classes = outcome_classes)

    outcome <- as.integer(outcome)
  } else {
    mode <- "regression"

    n_classes <- NULL

    outcome <- as.double(outcome)
  }

  if (verbose) {
    rlang::inform(paste0("mode: ", mode))
  }

  # Prepare validation data if needed
  if (validation_prop > 0) {
    in_val <- sample(seq_along(outcome), floor(length(outcome) * validation_prop))
    predictors_val <- predictors[in_val, , drop = FALSE]
    outcome_val <- outcome[in_val]

    predictors <- predictors[-in_val, , drop = FALSE]
    outcome <- outcome[-in_val]

    # scale the outcome
    if (mode == "regression") {
      outcome_info <- list(mean = mean(outcome), sd = stats::sd(outcome))
      outcome <- scale_y(outcome, outcome_info)
      outcome_val <- scale_y(outcome_val, outcome_info)
    }

    # Build val dataset
    ds_val <- build_wand_dataset(predictors_val, outcome_val, smooth_specs, requires_grad = T)
  } else {
    if (mode == "regression") {
      outcome_info <- list(mean = mean(outcome), sd = stats::sd(outcome))
      outcome <- scale_y(outcome, outcome_info)
    }
  }

  # Build dataset and dataloader
  ds <- build_wand_dataset(predictors, outcome, smooth_specs, requires_grad = T)
  dl <- torch::dataloader(ds, batch_size = batch_size)

  # Initialize wand modules
  if ("x_linear" %in% names(ds$tensors))
    n_linear_features <- ncol(ds$tensors$x_linear)
  else
    n_linear_features <- 0
  model <- wand_module(n_linear_features, smooth_specs, mode = mode, n_classes = n_classes)

  # Choose loss function
  if (mode == "classification") {
    loss_fn <- function(input, target) {
      torch::nnf_nll_loss(torch::torch_log(input), target)
    }
  } else if (mode == "regression") {
    # assumes 1d prediction
    loss_fn <- function(input, target) {
      torch::nnf_mse_loss(input[, 1], target)
    }
  }
  # Initialize optimizer
  # optimizer <- torch::optim_sgd(model$parameters, lr = learn_rate)
  optimizer <- torch::optim_adam(model$parameters, lr = learn_rate, weight_decay = 0.1)

  # Store results of the training loop
  loss_min <- 10^38
  loss_vec <- rep(NA_real_, epochs)
  val_loss_vec <- rep(NA_real_, epochs)
  val_loss_current <- NA
  loss_tol <- 5
  consec_iters_without_improvement <- 0

  # Iterate over epochs
  for (epoch in 1:epochs) {
    # Iterate over batches
    coro::loop(
      for (batch in dl) {
        pred <- model(batch)
        loss <- loss_fn(pred, batch$y)

        optimizer$zero_grad()
        loss$backward()
        optimizer$step()
      }
    )

    # Save loss and track whether or not loss is improving
    loss_current <- round(loss$item(), loss_tol)
    loss_vec[epoch] <- loss_current
    if (validation_prop > 0) {
      pred_val <- model(ds_val$tensors)
      loss <- loss_fn(pred_val, ds_val$tensors$y)
      val_loss_current <- round(loss$item(), loss_tol)
      val_loss_vec[epoch] <- val_loss_current

      if (val_loss_current < loss_min) {
        loss_min <- val_loss_current
        consec_iters_without_improvement <- 0
        best_model_params <- lapply(model$state_dict(), torch::as_array)
      } else {
        consec_iters_without_improvement <- consec_iters_without_improvement + 1
      }
    } else {
      if (loss_current < loss_min) {
        loss_min <- loss_current
        consec_iters_without_improvement <- 0
        best_model_params <- lapply(model$state_dict(), torch::as_array)
      } else {
        consec_iters_without_improvement <- consec_iters_without_improvement + 1
      }

    }

    # Report back to the user
    if (verbose) {
      rlang::inform(paste0("epoch: ", epoch,
                           ", loss: ", loss_current,
                           ", validation loss: ", val_loss_current))
    }

    # Stop if there's no improvement
    if (consec_iters_without_improvement >= stop_iter) {
      rlang::inform(paste0("No improvement for the last ",
                           consec_iters_without_improvement,
                           " epochs. Stopping."))
      break()
    }
  }

  # return
  list(
    model_obj = dehydrate_model(model),
    best_model_params = best_model_params,
    loss = loss_vec,
    validation_loss = val_loss_vec,
    smooth_specs = if (rlang::is_missing(smooth_specs)) list() else smooth_specs,
    training_params = list(batch_size = batch_size,
                           validation_prop = validation_prop,
                           epochs = epochs,
                           learn_rate = learn_rate,
                           stop_iter = stop_iter),
    outcome_info = outcome_info,
    mode = mode
  )
}
