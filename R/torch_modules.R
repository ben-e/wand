# Smooth modules -----------------------------------------------------------------------------------
# Smooth modules are wrapped by s_ functions which are meant to be user-facing.

wand_mlp_module <- torch::nn_module(
  "wand_mlp_module",
  initialize = function(n_features, hidden_units) {
    layers <- list()

    # input layer
    layers[[1]] <- torch::nn_linear(n_features, hidden_units[1])
    layers[[2]] <- torch::nn_relu()

    n_layers <- length(hidden_units)

    # hidden layers and output
    for (i in seq_along(hidden_units)[-1]) {
      layers[[length(layers) + 1]] <- torch::nn_linear(hidden_units[i - 1], hidden_units[i])
      layers[[length(layers) + 1]] <- torch::nn_relu()
    }

    # model
    self$model <- torch::nn_sequential(!!!layers)
  },
  forward = function(x) {
    self$model(x)
  }
)

#' @export
s_mlp <- function(..., hidden_units = c(32, 32, 32)) {
  features <- rlang::enquos(...)

  list(
    features = features,
    torch_module = wand_mlp_module,
    torch_module_parameters = list(n_features = length(features),
                                   hidden_units = hidden_units),
    n_smooth_features = tail(hidden_units, 1)
  )
}

# Output modules -----------------------------------------------------------------------------------
# These modules take all linear and smooth features and generate output.

wand_regression_module <- torch::nn_module(
  "wand_regression_module",
  initialize = function(n_features) {
    self$model <- torch::nn_linear(n_features, 1)
  },
  forward = function(x) {
    self$model(x)
  }
)

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
# The wand module acts as a container for the smoothing and output modules.

wand_module <- torch::nn_module(
  "wand_module",
  initialize = function(n_linear_features, smooth_specs, mode, n_classes) {
    # Smooth modules
    if (rlang::is_missing(smooth_specs)) {
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
