#' Convert a matrix to a `wand` compatible `torch::dataset`.
build_wand_dataset <- function(x, y, smooth_features) {
  # Create dataset with linear features and outcome

  # TODO This prevents users from doing something like including a feature as a smooth and linear
  # term (might be useful when the user is specifying an interaction), the easiest workaround is
  # for users to just wrap the linear feature in I, or to create their own duplicate column, but
  # I should figure out a standardized approach.

  if (!missing(smooth_features)) {
    smooth_feature_names <- unlist(sapply(smooth_features,
                                          \(x) sapply(x$features, \(y) rlang::quo_name(y))))
    linear_feature_names <- setdiff(names(x), smooth_feature_names)
  } else {
    linear_feature_names <- names(x)
  }

  ds <- torch::tensor_dataset(
    x_linear = torch::torch_tensor(as.matrix(dplyr::select(x, !!linear_feature_names))),
    y = torch::torch_tensor(as.matrix(y))
  )

  # Add smooth feature tensors
  if (!missing(smooth_features) && length(smooth_features) > 0) {
    for (i in 1:length(smooth_features)) {
      ds$tensors[[names(smooth_features)[i]]] <- torch::torch_tensor(
        as.matrix(dplyr::select(x, !!!smooth_features[[i]]$features))
      )
    }
  }
  ds
}
