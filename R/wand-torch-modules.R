# Linear module ------------------------------------------------------------------------------------
# This is wand's output module.

wand_linear_module <- torch::nn_module(
  "wand_linear_module",
  initialize = function(n_features) {
    self$output <- torch::nn_linear(n_features, 1, bias = T)
  },
  forward = function(x) {
    self$output(x)
  }
)
# MLP module ---------------------------------------------------------------------------------------
# Used to fit smooth terms using a multilayer perceptron.

wand_mlp_module <- torch::nn_module(
  "wand_mlp_module",
  initialize = function(n_features, hidden_units) {
    # list to hold layers
    layers <- list()

    # input layer
    layers[[1]] <- torch::nn_linear(n_features, hidden_units[1])
    layers[[2]] <- torch::nn_relu()

    # hidden layers
    for (i in 2:(length(hidden_units) - 1)) {
      layers[[length(layers) + 1]] <- torch::nn_linear(hidden_units[i - 1], hidden_units[i])
      layers[[length(layers) + 1]] <- torch::nn_relu()
    }

    # output layer
    layers[[length(layers) + 1]] <- torch::nn_linear(hidden_units[length(hidden_units) - 1],
                                                     hidden_units[length(hidden_units)])

    # model
    self$model <- torch::nn_sequential(!!!layers)
  },
  forward = function(x) {
    self$model(x)
  }
)

# wand module --------------------------------------------------------------------------------------
# This module initializes and uses the previous modules as needed.

wand_module <- torch::nn_module(
  "wand_module",
  initialize = function(n_linear_features, hidden_units) {

    # Create an mlp module for each smooth feature
    n_smooth_features <- length(hidden_units)
    if (n_smooth_features > 0) {
      self$mlp_modules <- torch::nn_module_list(lapply(1:n_smooth_features,
                                                       \(x) wand_mlp_module(1, hidden_units[[x]])))
    } else {
      self$mlp_modules <- torch::nn_module_list()
    }

    # Create the linear module
    # number of features = n_linear + n_smooth*number of outputs from the mlps
    # torch takes care of the bias terms
    # get number of features coming out of the mlp module by grabbing the last entry in each vector
    n_smooth_features_out <- sum(vapply(hidden_units, \(x) tail(x, 1), 0))
    self$linear_module <- wand_linear_module(n_linear_features + n_smooth_features_out)
  },
  forward = function(x_linear, x_smooth) {
    # Get smooth features
    n_smooth_features <- length(self$mlp_modules)
    if (n_smooth_features > 0) {
      x_smooth_out <- lapply(1:n_smooth_features,
                             \(i) self$mlp_modules[[i]](x_smooth[ , i, drop = F]))
      x_smooth_out <- torch::torch_cat(x_smooth_out, dim = 2)
    } else {
      x_smooth_out <- torch::torch_empty(nrow(x_linear), 0)
    }

    # Concat smooth features with linear features
    x <- torch::torch_cat(list(x_linear, x_smooth_out), dim = 2)

    # Linear output
    self$linear_module(x)
  }
)
