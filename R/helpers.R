# Builds a wand dataset where smoothed features are stored in separate tensors.
build_wand_dataset <- function(x, y, smooth_specs, requires_grad = T) {
  # Get the names of linear and to-be smoothed features
  if(rlang::is_missing(smooth_specs)) {
    smooth_features <- c()
  } else {
    smooth_features <- sapply(smooth_specs, \(x) sapply(x$features, \(y) rlang::quo_name(y)))
    smooth_features <- unlist(smooth_features)
  }
  linear_features <- setdiff(colnames(x), smooth_features)

  # Create tensors to be added to the tensor dataset
  ds_tensors <- list()

  # Linear features
  if (length(linear_features) > 0) {
    ds_tensors[["x_linear"]] <- torch::torch_tensor(as.matrix(x[ , linear_features]),
                                                    requires_grad = requires_grad)
  }

  # to-be smoothed features
  if (!rlang::is_missing(smooth_specs)) {
    for (i in seq_along(smooth_specs)) {
      ds_tensors[[names(smooth_specs)[i]]] <- torch::torch_tensor(
        # TODO This transmute is performing computations (e.g. log(x)) that should probably be
        # done using mold/blueprints?
        as.matrix(dplyr::transmute(x, !!!smooth_specs[[i]]$features)),
        requires_grad = requires_grad
      )
    }
  }

  # outcome
  # Note that we won't have an outcome when making predictions. As such, y is not a required arg.
  if(!rlang::is_missing(y)) {
    ds_tensors[["y"]] <- torch::torch_tensor(as.vector(y))
  }

  # Create the tensor dataset from all tensors and return
  torch::tensor_dataset(!!!ds_tensors)
}

#' Save models to raw objects.
#'
#' Directly from brulee.
dehydrate_model <- function(model) {
  con <- rawConnection(raw(), open = "w")
  on.exit({close(con)}, add = TRUE)
  torch::torch_save(model, con)
  r <- rawConnectionValue(con)
  r
}

#' Load models from raw objects.
#'
#' Also directly from brulee.
hydrate_model <- function(model) {
  con <- rawConnection(model)
  on.exit({close(con)}, add = TRUE)
  module <- torch::torch_load(con)
  module
}
