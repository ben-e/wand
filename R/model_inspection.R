# Recommended here: https://github.com/r-lib/tidyselect/issues/201
# May be deprecated soon
utils::globalVariables("where")

#' Plot model loss by epoch
#'
#' @param wand_fit A fitted `wand` model.
#' @param show_best_epoch Logical indicating whether or not a vline indicating the best training
#'   epoch should be included. If `validation_prop > 0` then this line will identify the best
#'   validation epoch, otherwise it will identify the best training epoch.
#' @param show_early_stopping Logical indicating whether or not a vline indicating the early
#'   stopping point should be included.
#' @param show_labels Logical indicating whether or not annotations labeling the best epoch and
#'   early stopping lines (if they are enabled) should be included.
#'
#' @return A ggplot.
#'
#' @export
wand_plot_loss <- function(wand_fit, show_best_epoch = T, show_early_stopping = T,
                           show_labels = T) {
  df <- rbind(
    data.frame(epoch = 1:wand_fit$training_params$epochs,
               loss = wand_fit$training_results$loss,
               type = "loss"),
    data.frame(epoch = 1:wand_fit$training_params$epochs,
               loss = wand_fit$training_results$validation_loss,
               type = "validation_loss")
  )
  df <- stats::na.omit(df)

  best_epoch <- wand_fit$training_results$best_epoch
  last_epoch <- wand_fit$training_results$last_epoch

  pl <- ggplot2::ggplot(df, ggplot2::aes_string(x = "epoch", y = "loss", colour = "type")) +
    ggplot2::geom_line() +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks())

  if (show_labels) {
    label_y <- stats::weighted.mean(c(max(df$loss, na.rm = T), min(df$loss, na.rm = T)),
                                    c(2/3, 1/3))
  }

  if (show_best_epoch) {
    pl <- pl +
      ggplot2::geom_vline(xintercept = best_epoch,
                          colour = "forestgreen",
                          alpha = 0.5, linetype = "dashed")

    if (show_labels) {
      pl <- pl +
        ggplot2::annotate(x = best_epoch, y = label_y, hjust = "inward",
                          geom = "label",
                          label = "Best epoch")
    }
  }

  if (show_early_stopping && last_epoch < wand_fit$training_params$epochs) {
    pl <- pl +
      ggplot2::geom_vline(xintercept = last_epoch,
                          colour = "firebrick1",
                          alpha = 0.5, linetype = "dashed")

    if (show_labels) {
      pl <- pl +
        ggplot2::annotate(x = last_epoch, y = label_y, hjust = "inward",
                          geom = "label",
                          label = "Early stopping")
    }
  }

  # TODO custom theme? custom color?
  pl
}

#' Plot smooth functions from a wand model
#'
#' @param wand_fit A fitted `wand` model.
#' @param seq_length The maximum number of points used when plotting each smooth. If the smooth is
#'   2d, this represents the number of unique points used for each axis, such that the total number
#'   of points is `seq_length*seq_length`.
#'
#' @return A list where each entry is a ggplot of a smooth function. Note that smooths are plotted
#' after any pre-processing.
#'
#' @export
wand_plot_smooths <- function(wand_fit, seq_length = 250) {
  # Get outcome name
  outcome <- names(wand_fit$blueprint$ptypes$outcomes)[1]

  if (wand_fit$mode == "classification") {
    prediction_mode <- "prob"
  } else if (wand_fit$mode == "regression") {
    prediction_mode <- "numeric"
  }

  if (max(sapply(wand_fit$smooth_specs, \(x) length(x$features))) > 2) {
    rlang::abort("Only smooths with a max of two features can currently be plotted.")
  }

  lapply(wand_fit$smooth_specs, \(smooth_spec) {
    # TODO probably a cleaner way to build this grid, maybe just use tidyr..

    smooth_features <- smooth_spec$features

    # Get the typical values for all predictors except the current smooth features
    typical_df <- wand_fit$predictor_info$typical_df
    typical_df <- typical_df[typical_df$.metric == "typical", ]
    typical_df <- dplyr::select(typical_df, -dplyr::all_of(c(smooth_features, ".metric")))

    # Create a grid across feature values
    dg_info <- dplyr::select(wand_fit$predictor_info$typical_df, dplyr::all_of(smooth_features))
    dg_info <- as.data.frame(t(dg_info))
    colnames(dg_info) <- wand_fit$predictor_info$typical_df$.metric
    dg <- lapply(seq_along(smooth_features),
                 \(i) seq(from = dg_info$min[i], to = dg_info$max[i], by = dg_info$spacing[i]))
    dg <- expand.grid(dg)
    colnames(dg) <- smooth_features

    # Combine typical features with the datagrid
    dg <- dplyr::tibble(typical_df, dg)

    # Get model predictions
    dg$.pred <- stats::predict(wand_fit, new_data = dg, type = prediction_mode)[[1]]

    # Plot, finally
    if (length(smooth_features) == 1) {
      pl <- ggplot2::ggplot(dg, ggplot2::aes_string(x = smooth_features[1], y = ".pred")) +
        ggplot2::geom_path()
    }
    else {
      pl <- ggplot2::ggplot(dg, ggplot2::aes_string(x = smooth_features[1],
                                                    y = smooth_features[2],
                                                    z = ".pred")) +
        ggplot2::stat_contour_filled() +
        ggplot2::labs(fill = ".pred")
    }

    pl
  })
}

#' Builds a `ggraph::tbl_graph` showing data flow through a wand model
#'
#' @param wand_fit A fitted `wand` model.
#'
#' @return A `ggraph::tbl_graph` graph of the model.
#'
#' @export
build_wand_graph <- function(wand_fit) {
  # Get feature names
  linear_features <- wand_fit$predictor_info$linear_predictors
  smooth_features <- wand_fit$predictor_info$smooth_predictors

  # Get outcome
  if (sapply(wand_fit$blueprint$ptypes$outcomes, is.factor)[1]) {
    outcomes <- levels(wand_fit$blueprint$ptypes$outcomes[[1]])
    outcomes <- paste0("Class: ", outcomes)
  } else {
    # Assumes only one outcome, also an assumption of the model
    outcomes <- names(wand_fit$blueprint$ptypes$outcomes)[1]
  }
  outcomes <- paste0("Predicted ", outcomes)

  # Get smooth modules
  smooth_modules <- sapply(wand_fit$smooth_specs, \(x) {
    paste0(#"features: ", paste0(names(x$predictors), collapse = ", "), "\n",
      "torch module: \n", x$torch_module$classname, "\n",
      "torch module parameters: \n",
      paste0(names(x$torch_module_parameters), ": ",
             as.character(x$torch_module_parameters),
             collapse = "\n")
    )
  })
  smooth_modules <- paste0(paste0(names(smooth_modules), "\n"), smooth_modules)

  # Create nodes, add linear layer
  nodes <- data.frame(name = c(linear_features, smooth_features,
                               smooth_modules, "linear_layer", outcomes))

  # Create edges
  smooth_edges <- data.frame(
    from = smooth_features,
    to = rep(smooth_modules,
             times = sapply(wand_fit$smooth_specs, \(x) length(x$features)))
  )
  linear_layer_edges <- data.frame(
    from = c(smooth_edges$to, linear_features),
    to = "linear_layer"
  )
  output_edges <- data.frame(
    from = "linear_layer",
    to = outcomes
  )

  # Create graph
  model_graph <- tidygraph::tbl_graph(
    nodes = nodes,
    edges = rbind(smooth_edges, linear_layer_edges, output_edges)
  )

  model_graph
}

#' Extract model coefficients
#'
#' @param object A fitted `wand` model.
#' @param ... Currently unused.
#'
#' @return A matrix of the final linear layer's weights and biases. Note that classification models
#'   use a softmax activation, which returns one column per class.
#'
#' @export
coef.wand <- function(object, ...) {
  model <- hydrate_model(object$model_obj)
  model_weight <- torch::as_array(model$linear_module$state_dict()[[1]])
  model_bias <- torch::as_array(model$linear_module$state_dict()[[2]])

  # Note that the weights vector includes all the weights on the smoothed features, we don't
  # actually want those, so remove them.
  n_features <- ncol(model_weight)
  if (length(object$predictor_info$smooth_predictors) > 0) {
    n_smooth_features <- sum(sapply(object$smooth_specs, \(x) x$n_smooth_features))
  } else {
    n_smooth_features <- 0
  }

  # The wand module starts with linear features
  if ((n_features - n_smooth_features) > 0) {
    model_weight <- model_weight[ , 1:(n_features - n_smooth_features), drop = F]
  } else {
    model_weight <- model_weight[ , 0, drop = F]
  }

  model_coefs <- rbind(model_bias, t(model_weight))
  rownames(model_coefs) <- c("(Intercept)", object$predictor_info$linear_predictors)
  if (object$mode == "classification")
    colnames(model_coefs) <- levels(object$blueprint$ptypes$outcomes[[1]])

  model_coefs
}
