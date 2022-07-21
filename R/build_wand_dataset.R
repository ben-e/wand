# Functions to convert data into torch/wand compatible formats -------------------------------------

#' Builds `wand` compatible `torch::tensor_dataset`s
#'
#' @param linear_predictors The linear predictors as an R __dataframe__ or __matrix__.
#' @param smooth_predictors A named list of R where each entry is a __dataframe__ or __matrix__
#'   representing a smooth predictor.
#' @param outcome The target as an R __vector__.
#' @param requires_grad A boolean indicating whether or not the prediction tensors need gradients.
#'   This can be turned off for datasets that are only used for evaluation, e.g. validation data or
#'   prediction data.
#'
build_wand_dataset <- function(linear_predictors, smooth_predictors, outcome, requires_grad = T) {
  # Create tensors to be added to the tensor dataset
  ds_tensors <- list()

  # Linear features
  if (length(linear_predictors) > 0) {
    ds_tensors[["x_linear"]] <- torch::torch_tensor(as.matrix(linear_predictors),
                                                    requires_grad = requires_grad)
  }

  # Deep/smooth features

  for (i in seq_along(smooth_predictors)) {
    ds_tensors[[names(smooth_predictors)[i]]] <- torch::torch_tensor(
      as.matrix(smooth_predictors[[i]]),
      requires_grad = requires_grad
    )
  }

  # outcome
  # Note that we won't have an outcome when making predictions. As such, y is not a required arg.
  if(!rlang::is_missing(outcome)) {
    ds_tensors[["y"]] <- torch::torch_tensor(as.vector(outcome))
  }

  # Create the tensor dataset from all tensors and return
  torch::tensor_dataset(!!!ds_tensors)
}
