#' Convert a matrix to a `wand` compatible `torch::dataset`.
matrix_to_wand_dataset <- function(x, y, smooth_feature_indices) {
  if (length(smooth_feature_indices) > 0) {
    torch::tensor_dataset(x_linear = torch::torch_tensor(x[ , -smooth_feature_indices]),
                          x_smooth = torch::torch_tensor(x[ , smooth_feature_indices]),
                          y = torch::torch_tensor(y))
  } else {
    torch::tensor_dataset(x_linear = torch::torch_tensor(x),
                          x_smooth = torch::torch_empty(nrow(x), 0),
                          y = torch::torch_tensor(y))
  }
}
