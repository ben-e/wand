#' Specify a smooth function using a multilayer perceptron
#'
#' This function specifies a smooth function using a multilayer perceptron (mlp). Note that this
#' function does not do the actual smoothing, it merely specifies the features to be smoothed,
#' the `torch::nn_module` module to be used to do the smoothing, in this case an MLP, and the
#' parameters needed to initialize that module.
#'
#' @param ... The unquoted variables to be smoothed.
#' @param hidden_units An integer vector specifying the number of hidden units in each layer of the
#'   mlp. Note that `length(hidden_units)` is the number of layers in the mlp; the final entry in
#'   `hidden_units` represents the output of this smoother.
#'
#' @return
#'
#' A named list containing the features to be smoothed, the torch module doing the smoothing,
#' the parameters needed to initialize that module, and the number of features returned by the mlp.
#'
#' @export
s_mlp <- function(..., hidden_units = c(32, 32, 32)) {
  formula <- dots_to_formula(...)
  validate_smooth_formula(formula)

  features <- attr(terms(formula), "term.labels")

  list(
    formula = formula,
    features = features,
    torch_module = wand_mlp_module,
    torch_module_parameters = list(n_features = length(features),
                                   hidden_units = hidden_units),
    n_smooth_features = utils::tail(hidden_units, 1)
  )
}

#' TODO should s_ functions be an S3 class?
#' TODO what about an s_ constructor function?
