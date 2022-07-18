#' Wrapper to add `wand` as an engine to parsnip's `gen_additive_model` model spec
#'
#' @return NULL
#'
#' @export
add_wand_gen_additive_mod <- function() {
  # Register with parsnip
  parsnip::set_model_engine("gen_additive_mod", mode = "regression", eng = "wand")
  parsnip::set_model_engine("gen_additive_mod", mode = "classification", eng = "wand")
  parsnip::set_dependency("gen_additive_mod", eng = "wand", pkg = "wand")

  # Fit
  parsnip::set_fit(
    model = "gen_additive_mod",
    eng = "wand",
    mode = "regression",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "wand", fun = "wand"),
      defaults = list()
    )
  )

  parsnip::set_fit(
    model = "gen_additive_mod",
    eng = "wand",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y"),
      func = c(pkg = "wand", fun = "wand"),
      defaults = list()
    )
  )

  # Encoding
  parsnip::set_encoding(
    model = "gen_additive_mod",
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
    model = "gen_additive_mod",
    eng = "wand",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  # Args
  parsnip::set_model_arg(
    model = "gen_additive_mod",
    eng = "wand",
    parsnip = "batch_size",
    original = "batch_size",
    func = list(pkg = "dials", fun = "batch_size"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "gen_additive_mod",
    eng = "wand",
    parsnip = "epochs",
    original = "epochs",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "gen_additive_mod",
    eng = "wand",
    parsnip = "learn_rate",
    original = "learn_rate",
    func = list(pkg = "dials", fun = "learn_rate"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "gen_additive_mod",
    eng = "wand",
    parsnip = "stop_iter",
    original = "stop_iter",
    func = list(pkg = "dials", fun = "stop_iter"),
    has_submodel = FALSE
  )

  # Predictions
  set_pred(
    model = "gen_additive_mod",
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

  set_pred(
    model = "gen_additive_mod",
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

  set_pred(
    model = "gen_additive_mod",
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

.onLoad <- function(libname, pkgname){
  # Registers with parsnip when wand is loaded
  add_wand_gen_additive_mod()
}
