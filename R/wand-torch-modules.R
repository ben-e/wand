# Smooth module ------------------------------------------------------------------------------------
# These modules are made for smoothing features.

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
    # TODO should output layer have an activation function? I don't see what utility it would
    # provide?
    layers[[length(layers) + 1]] <- torch::nn_linear(hidden_units[length(hidden_units) - 1],
                                                     hidden_units[length(hidden_units)])

    # model
    self$model <- torch::nn_sequential(!!!layers)
  },
  forward = function(x) {
    self$model(x)
  }
)

# Smooth module wrappers ---------------------------------------------------------------------------
# These are wrappers around torch modules, meant to be user facing.

#' @export
s_mlp <- function(..., hidden_units = c(32, 32, 32)) {
  features <- rlang::enquos(...)

  list(
    # features that are being smoothed
    features = features,
    # torch module doing the smoothing
    torch_module = wand_mlp_module,
    # parameters passed to the module
    torch_module_parameters = list(n_features = length(features),
                                   hidden_units = hidden_units)
  )
}

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

# wand module --------------------------------------------------------------------------------------
# This module acts as a container for all smoothing modules and the final linear output module.

wand_module <- torch::nn_module(
  "wand_module",
  initialize = function(n_linear_features, smooth_features) {
    # Create smooth feature modules
    self$smooth_modules <- torch::nn_module_list()
    self$smooth_module_names <- c()
    n_smooth_features_out <- 0

    if (!missing(smooth_features) && length(smooth_features) > 0) {
      for (i in 1:length(smooth_features)) {
        smooth_module_name <- names(smooth_features)[i]
        self$smooth_module_names <- c(self$smooth_module_names, smooth_module_name)

        self$smooth_modules$add_module(
          smooth_module_name,
          rlang::exec(smooth_features[[i]]$torch_module,
                      !!!smooth_features[[i]]$torch_module_parameters)
        )

        # Track the number of features in the output layer, assumes that the output is 1d
        current_n_smooth_features_out <-
          tail(self$smooth_modules[[smooth_module_name]]$parameters, 1)[[1]]$shape[1]
        n_smooth_features_out <- n_smooth_features_out + current_n_smooth_features_out
      }
    }

    # Create the linear module
    # number of features = n_linear + number of outputs from the smooth modules
    self$linear_module <- wand_linear_module(n_linear_features + n_smooth_features_out)
  },
  forward = function(x) {
    # Forward through smooth modules
    x_smooth_out <- torch::torch_empty(nrow(x$x_linear), 0)
    for (smooth_module in self$smooth_module_names) {
      x_smooth_out <- torch::torch_cat(
        list(x_smooth_out,
             self$smooth_modules[[smooth_module]](x[[smooth_module]])),
        dim = 2
      )
    }

    # Concat smooth features with linear features
    x <- torch::torch_cat(list(x$x_linear, x_smooth_out), dim = 2)

    # Linear output
    self$linear_module(x)
  }
)
