#' Add `nn_additive_mod` as a model spec and `wand` as engine to parsnip
#'
#' @note Ideally, `wand` should fall under `gen_additve_mod`, but I've had issues working
#' around the assumptions that parsnip makes about this model spec. I will try to incorporate
#' `wand` with `nn_additive_mod` in the future.
#'
#' @return NULL
#'
#' @export
add_nn_additive_mod <- function() {
  # Set up a new model spec
  parsnip::set_new_model("nn_additive_mod")
  parsnip::set_model_mode("nn_additive_mod", "regression")
  parsnip::set_model_mode("nn_additive_mod", "classification")

  # Set up wand as an engine
  parsnip::set_model_engine("nn_additive_mod", mode = "regression", eng = "wand")
  parsnip::set_model_engine("nn_additive_mod", mode = "classification", eng = "wand")
  parsnip::set_dependency("nn_additive_mod", eng = "wand", pkg = "wand")

  # Fit
  parsnip::set_fit(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "wand", fun = "wand"),
      defaults = list(smooth_specs = rlang::expr(list()),
                      verbose = rlang::expr(F))
    )
  )

  parsnip::set_fit(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "wand", fun = "wand"),
      defaults = list(smooth_specs = rlang::expr(list()),
                      verbose = rlang::expr(F))
    )
  )

  # Encoding
  parsnip::set_encoding(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_encoding(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_model_arg(
    model = "nn_additive_mod",
    eng = "wand",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(pkg = "dials", fun = "batch_size"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nn_additive_mod",
    eng = "wand",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nn_additive_mod",
    eng = "wand",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "nn_additive_mod",
    eng = "wand",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  # Predictions
  parsnip::set_pred(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "numeric"
        )
    )
  )

  parsnip::set_pred(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "class"
        )
    )
  )

  parsnip::set_pred(
    model = "nn_additive_mod",
    eng = "wand",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "prob"
        )
    )
  )
}

#' Generalized additive model via neural networks
#'
#' @inheritParams wand
#' @param mode A single character string for the prediction outcome mode. Possible values for this
#'   model are "unknown", "regression", or "classification".
#' @param engine A single character string specifying what computational engine to use for fitting.
#'
#' @export
nn_additive_mod <- function(mode = "unknown",
                            engine = "wand",
                            batch_size = NULL,
                            epochs = NULL,
                            learn_rate = NULL,
                            stop_iter = NULL,
                            smooth_specs = NULL) {
  args <- list(
    batch_size = rlang::enquo(batch_size),
    epochs = rlang::enquo(epochs),
    learn_rate = rlang::enquo(learn_rate),
    stop_iter = rlang::enquo(stop_iter),
    smooth_specs = rlang::enquo(smooth_specs)
  )

  parsnip::new_model_spec(
    "nn_additive_mod",
    args     = args,
    eng_args = NULL,
    mode     = mode,
    method   = NULL,
    engine   = engine
  )
}

.onLoad <- function(libname, pkgname){
  # Registers with parsnip when wand is loaded
  add_nn_additive_mod()
}
