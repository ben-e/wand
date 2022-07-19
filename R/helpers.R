#' Builds `wand` compatible `torch::tensor_dataset`s
#'
#' @param x The predictors as an R __dataframe__ or __matrix__, must include the features used in
#'   the `smooth_specs` specifications.
#' @param y The model target as an R __vector__.
#' @param smooth_specs A named list of smooth specifications.
#' @param requires_grad A boolean indicating whether or not the `x` tensors will need gradients.
#'   This can be turned off for datasets that are only used for evaluation, e.g. validation data.
#'
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

#' Store a `torch` module as a raw R object
#'
#' This function is used to store `torch` modules, usually after training, as raw R objects. This
#' function comes directly from the `brulee` package.
#'
#' @param model A `torch` module.
#'
#' @return A raw object.
dehydrate_model <- function(model) {
  con <- rawConnection(raw(), open = "w")
  on.exit({close(con)}, add = TRUE)
  torch::torch_save(model, con)
  r <- rawConnectionValue(con)
  r
}

#' Load models from raw objects.
#'
#' This function is used to restore dehydrated `torch` modules from raw R objects. This function
#' comes directly from the `brulee` package.
#'
#' @param model A raw object representing a `torch` module.
#'
#' @return A `torch` module.
hydrate_model <- function(model) {
  con <- rawConnection(model)
  on.exit({close(con)}, add = TRUE)
  module <- torch::torch_load(con)
  module
}

#' Scales an outcome given a list containing mean and sd.
#'
#' This function comes from brulee. It is used to scale the outcome before training and prediction.
#'
#' @param y The numeric outcome of interest.
#' @param stats A named list containing `mean` and `sd` entries.
#'
#' @return The scaled `y` value
scale_y <- function(y, stats) {
  (y - stats$mean)/stats$sd
}
