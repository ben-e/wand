# Smooth modules -----------------------------------------------------------------------------------

#' A multilayer perceptron `torch::nn_module` module
#'
#' @param n_features An integer giving the number of input features.
#' @param hidden_units An integer vector giving the size of each layer. The final element gives the
#'   number of features returned by the module.
#'
#' @details
#'
#' This module creates a very simple multilayer perceptron (mlp). The mlp uses the `torch::nn_selu`
#' activation function, no dropout, and no batchnormalization. Note that the activation function is
#' applied to the returned features.
#'
#' @return A multilayer perceptron `torch::nn_module` module
#'
#' @export
wand_mlp_module <- torch::nn_module(
  "wand_mlp_module",
  initialize = function(n_features, hidden_units) {
    layers <- list()

    # input layer
    layers[[1]] <- torch::nn_linear(n_features, hidden_units[1])
    layers[[2]] <- torch::nn_selu()

    n_layers <- length(hidden_units)

    # hidden layers and output
    for (i in seq_along(hidden_units)[-1]) {
      layers[[length(layers) + 1]] <- torch::nn_linear(hidden_units[i - 1], hidden_units[i])
      if (i < n_layers)
        layers[[length(layers) + 1]] <- torch::nn_selu()
    }

    # model
    self$model <- torch::nn_sequential(!!!layers)
  },
  forward = function(x) {
    self$model(x)
  }
)


# Output modules -----------------------------------------------------------------------------------
# These modules take all linear and smooth features and generate output.

#' A linear regression `torch::nn_module` module
#'
#' This module takes in `n_features` and returns a single feature, with no activation.
#'
#' @param n_features An integer giving the number of input features. Used to initialize the module.
#'
#' @return A `torch::nn_module` module which returns a single feature with no activation.
wand_regression_module <- torch::nn_module(
  "wand_regression_module",
  initialize = function(n_features) {
    self$model <- torch::nn_linear(n_features, 1)
  },
  forward = function(x) {
    self$model(x)
  }
)

#' A linear regression `torch::nn_module` module for classification
#'
#' This module takes in `n_features` and returns `n_classes` values using a `torch::nn_softmax`
#' activation.
#'
#' @param n_features An integer giving the number of input features. Used to initialize the module.
#' @param n_classes An integer giving the number of output classes Used to initialize the module.
#'
#' @return A `torch::nn_module` module which returns a returns `n_classes` values using a
#'   `torch::nn_softmax` activation.
wand_classification_module <- torch::nn_module(
  "wand_classification_module",
  initialize = function(n_features, n_classes) {
    self$model <- torch::nn_sequential(
      torch::nn_linear(n_features, n_classes),
      torch::nn_softmax(dim = 2)
    )
  },
  forward = function(x) {
    self$model(x)
  }
)

# wand module --------------------------------------------------------------------------------------
# The wand module acts as a container for the deep and linear output modules.

#' A `torch::nn_module` module used to fit wide and deep neural networks
#'
#' This module is not meant to be used directly by users. It makes many assumptions about inputs,
#' and is used as the main engine of the `wand` package.
#'
#' @param n_linear_features An integer giving the number of linear features.
#' @param smooth_specs A named list of smooth specifications, e.g. `s_mlp`.
#' @param mode A string giving the model's goal: "classification" or "regression".
#' @param n_classes An integer giving the number of classes, required when
#'   `mode == "classification"`.
#'
wand_module <- torch::nn_module(
  "wand_module",
  initialize = function(n_linear_features, smooth_specs, mode, n_classes) {
    # Smooth modules
    if (rlang::is_missing(smooth_specs) || length(smooth_specs) == 0) {
      n_smooth_features <- 0
      self$smooth_modules <- torch::nn_module_list()
    } else {
      n_smooth_features <- sum(sapply(smooth_specs, \(x) x$n_smooth_features))
      self$smooth_modules <- torch::nn_module_list(
        lapply(smooth_specs, \(x) rlang::exec(x$torch_module, !!!x$torch_module_parameters))
      )
      self$smooth_module_names <- names(smooth_specs)
    }

    # Linear modules
    if (rlang::is_missing(mode)) {
      rlang::abort("`mode` is a required argument.")
    } else if (mode == "regression") {
      self$linear_module <- wand_regression_module(n_linear_features + n_smooth_features)
    } else if (mode == "classification") {
      if (rlang::is_missing(n_classes)) {
        rlang::abort("`n_classes` is a required argument for classification.")
      }
      self$linear_module <- wand_classification_module(n_linear_features + n_smooth_features,
                                                       n_classes = n_classes)
    } else {
      rlang::abort(paste0("`", mode, "`", "is not a valid value for `mode`."))
    }
  },
  forward = function(x) {
    # Add smooth features to the tensor to be passed through the linear module
    x_all <- torch::torch_cat(append(
      x$x_linear,
      lapply(seq_along(self$smooth_modules),
             \(i) self$smooth_modules[[i]](x[[self$smooth_module_names[i]]]))
    ), dim = 2)
    # pass through linear module
    self$linear_module(x_all)
  }
)
