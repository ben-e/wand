#' Store a `torch` module as a raw R object.
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

#' Validate a formula for a smooth specification.
validate_smooth_formula <- function(formula) {
  if (!is.null(rlang::f_lhs(formula)))
    rlang::abort("Formulas for smooth specifications can not contain a left hand side.")

  formula
}

#' Combine ... into a RHS only formula
dots_to_formula <- function(...) {
  dots <- rlang::enquos(...)
  as.formula(paste0(" ~ ", paste0(sapply(dots, rlang::quo_name), collapse = " + ")))
}

#' Takes a formula, and replaces any smooth specs with the corresponding formula. Returns the
#' updated formula and the smooth specs extracted from the formula
extract_smooths <- function(formula) {
  smooth_specs <- list()
  for (term in attr(stats::terms(formula), "term.labels")) {
    # TODO I don't like that this assumes/requires that smoothers start with s_, perhaps
    # use a registry of smoothers? See parsnip's tracking of available models as an alternative.
    if (grepl("^s_", term)) {
      # get the actual smooth specification by evaluating the s function and save
      smooth_specs[[length(smooth_specs) + 1]] <- eval(parse(text = term))
      # update formula to remove the smooth
      formula <- update.formula(
        formula,
        paste0(c(
          ". ~ .",
          " - ", term,
          " + ",  rlang::f_rhs(smooth_specs[[length(smooth_specs)]]$formula)
        ), collapse = "")
      )
    }
  }

  list(formula = formula,
       smooth_specs = smooth_specs)
}
