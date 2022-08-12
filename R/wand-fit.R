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
#'   mod3 <- wand(rec, mtcars, smooth_specs = list(hp = s_mlp(hp), disp = s_mlp(disp)))
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
wand.data.frame <- function(x, y,
                            smooth_specs = list(),
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

  # TODO make this nicer
  typical_df <- dplyr::summarise(x, dplyr::across(.fns = typical))
  typical_df$.metric <- "typical"
  spacing_df <- dplyr::summarise(x, dplyr::across(where(is.numeric), spacing))
  spacing_df$.metric <- "spacing"
  min_df <- dplyr::summarise(x, dplyr::across(where(is.numeric), min))
  min_df$.metric <- "min"
  max_df <- dplyr::summarise(x, dplyr::across(where(is.numeric), max))
  max_df$.metric <- "max"
  typical_df <- dplyr::bind_rows(typical_df, spacing_df, min_df, max_df)

  processed <- hardhat::mold(x, y)

  smooth_specs <- lapply(
    smooth_specs,
    \(spec) append(spec, list(processed = hardhat::mold(spec$formula, data = x)))
  )

  wand_bridge(
    processed,
    smooth_specs,
    typical_df,
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
wand.matrix <- function(x, y,
                        smooth_specs = list(),
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

  if (is.null(colnames(x)))
    rlang::abort("`x` must have column names.")

  if (!is.numeric(x))
    rlang::abort("`x` must be numeric.")

  # TODO these are ugly
  typical_df <- as.data.frame(t(apply(x, 2, typical)))
  typical_df$.metric <- "typical"
  spacing_df <- as.data.frame(t(apply(x, 2, spacing)))
  spacing_df$.metric <- "spacing"
  min_df <- as.data.frame(t(apply(x, 2, min)))
  min_df$.metric <- "min"
  max_df <- as.data.frame(t(apply(x, 2, max)))
  max_df$.metric <- "max"
  typical_df <- dplyr::bind_rows(typical_df, spacing_df, min_df, max_df)

  processed <- hardhat::mold(x, y)

  smooth_specs <- lapply(
    smooth_specs,
    \(spec) append(spec, list(processed = hardhat::mold(spec$formula, data = x)))
  )

  wand_bridge(
    processed,
    smooth_specs,
    typical_df,
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

  typical_df <- dplyr::summarise(data, dplyr::across(.fns = typical))
  typical_df$.metric <- "typical"
  spacing_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), spacing))
  spacing_df$.metric <- "spacing"
  min_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), min))
  min_df$.metric <- "min"
  max_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), max))
  max_df$.metric <- "max"
  typical_df <- dplyr::bind_rows(typical_df, spacing_df, min_df, max_df)

  # Extract smooth terms from the formula
  new_formula_and_smooth_specs <- extract_smooths(formula)
  processed <- hardhat::mold(new_formula_and_smooth_specs$formula, data)

  smooth_specs <- lapply(
    new_formula_and_smooth_specs$smooth_specs,
    \(spec) append(spec, list(processed = hardhat::mold(spec$formula, data = data)))
  )

  wand_bridge(
    processed,
    smooth_specs,
    typical_df,
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
                        smooth_specs = list(),
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

  typical_df <- dplyr::summarise(data, dplyr::across(.fns = typical))
  typical_df$.metric <- "typical"
  spacing_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), spacing))
  spacing_df$.metric <- "spacing"
  min_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), min))
  min_df$.metric <- "min"
  max_df <- dplyr::summarise(data, dplyr::across(where(is.numeric), max))
  max_df$.metric <- "max"
  typical_df <- dplyr::bind_rows(typical_df, spacing_df, min_df, max_df)

  processed <- hardhat::mold(x, data)

  # unlike the other methods, the smooth specs are molded on the already processed data
  smooth_specs <- lapply(
    smooth_specs,
    \(spec) append(spec, list(processed = hardhat::mold(spec$formula, data = processed$predictors)))
  )

  wand_bridge(
    processed,
    smooth_specs,
    typical_df,
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

wand_bridge <- function(processed,
                        smooth_specs,
                        typical_df,
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

  # If a term is used in a smooth, remove it from the linear predictors
  # TODO For XY and recipe, if a user does something like s_mlp(log(z)) then z will be included
  # as a linear feature and log(z) will be included as a smooth feature. What do? Warn users they
  # should preproc transformations like this for these interfaces?
  smooth_predictors <- unique(unname(unlist(lapply(smooth_specs, \(i) i$features))))
  linear_predictors <- setdiff(names(processed$predictors), smooth_predictors)

  # make sure smooths are named
  if (!rlang::is_missing(smooth_specs) && is.null(names(smooth_specs)) && length(smooth_specs) > 0)
    names(smooth_specs) <- paste0("smooth_", 1:length(smooth_specs))

  # Fit the model
  fit <- wand_impl(
    # data
    dplyr::select(processed$predictors, dplyr::all_of(linear_predictors)),
    processed$outcomes[[1]],
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
    model_params = fit$best_model_params,
    outcome_info = fit$outcome_info,
    predictor_info = list(linear_predictors = linear_predictors,
                          smooth_predictors = smooth_predictors,
                          # TODO contains the outcome as well, so not..just predictors
                          typical_df = typical_df),
    mode = fit$mode,
    blueprint = processed$blueprint,
    smooth_blueprints = lapply(smooth_specs, \(x) x$processed$blueprint),
    training_params = fit$training_params,
    smooth_specs = smooth_specs,
    training_results = fit$training_results
  )
}


# --------------------------------------------------------------------------------------------------
# Implementation

wand_impl <- function(x,
                      y,
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
  if (is.factor(y)) {
    mode <- "classification"

    outcome_classes <- levels(y)
    n_classes <- length(outcome_classes)

    outcome_info <- list(classes = outcome_classes)

    y <- as.integer(y)
  } else {
    mode <- "regression"

    n_classes <- NULL

    y <- as.double(y)

    # scale the outcome
    if (validation_prop > 0) {
      in_val <- sample(seq_along(y), floor(length(y) * validation_prop))

      outcome_info <- list(mean = mean(y[-in_val]), sd = stats::sd(y[-in_val]))

      y <- scale_y(y, outcome_info)
    } else {
      outcome_info <- list(mean = mean(y), sd = stats::sd(y))
      y <- scale_y(y, outcome_info)
    }
  }

  if (verbose) {
    rlang::inform(paste0("mode: ", mode))
  }

  # Build the torch dataset
  if (validation_prop > 0) {
    ds_val <- build_wand_dataset(x[in_val, ,],
                                 lapply(smooth_specs, \(spec) spec$processed$predictors[in_val, ]),
                                 y[in_val],
                                 requires_grad = F)

    ds <- build_wand_dataset(x[-in_val, ,],
                             lapply(smooth_specs, \(spec) spec$processed$predictors[-in_val, ]),
                             y[-in_val],
                             requires_grad = T)
  } else {
    ds <- build_wand_dataset(x,
                             lapply(smooth_specs, \(spec) spec$processed$predictors),
                             y,
                             requires_grad = T)
  }

  # Build batch dataloader
  dl <- torch::dataloader(ds, batch_size = batch_size)

  # Initialize wand modules
  # TODO at this point the smooth specs also include a bunch of data that doesn't need to be passed
  # to wand module init, is this slow?
  model <- wand_module(ncol(x), smooth_specs, mode = mode, n_classes = n_classes)

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
  best_epoch <- 0

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
        best_epoch <- epoch
        consec_iters_without_improvement <- 0
        best_model_params <- lapply(model$state_dict(), torch::as_array)
      } else {
        consec_iters_without_improvement <- consec_iters_without_improvement + 1
      }
    } else {
      if (loss_current < loss_min) {
        loss_min <- loss_current
        best_epoch <- epoch
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
    training_params = list(batch_size = batch_size,
                           validation_prop = validation_prop,
                           epochs = epochs,
                           learn_rate = learn_rate,
                           stop_iter = stop_iter),
    training_results = list(loss = loss_vec,
                            validation_loss = val_loss_vec,
                            best_epoch = best_epoch,
                            last_epoch = epoch),
    outcome_info = outcome_info,
    mode = mode
  )
}
