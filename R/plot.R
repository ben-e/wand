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

  if (show_early_stopping && last_epoch < max(df$epoch)) {
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

#' Builds a `ggraph::tbl_graph` showing data flow through a wand model
#'
#' @param wand_fit A fitted `wand` model.
#'
#' @return A `ggraph::tbl_graph` graph of the model.
#'
#' @export
build_wand_graph <- function(wand_fit) {
  # Get all features
  features <- names(wand_fit$blueprint$ptypes$predictors)

  # Get smooth features and smooth modules
  smooth_nodes <- lapply(wand_fit$smooth_specs, \(x) {
    paste0(x$torch_module, "\n",
           "features: ", paste0(x$features, collapse = ", "), "\n",
           paste0(names(x$torch_module_parameters), ": ",
                  as.character(x$torch_module_parameters),
                  collapse = "\n")
    )
  })
  smooth_features <- lapply(wand_fit$smooth_specs, \(x) names(x$blueprint$ptypes$predictors))
  names(smooth_features) <- smooth_nodes

  # Linear features
  linear_features <- setdiff(features, unlist(smooth_features))

  # Get outcome
  # If the outcome is a factor, then we care about the levels
  if (is.factor(wand_fit$blueprint$ptypes$outcomes[[1]])) {
    outcomes <- levels(wand_fit$blueprint$ptypes$outcomes[[1]])
    outcomes <- paste0("Class: ", outcomes)
  } else {
    # Assumes only one outcome, also an assumption of the model
    outcomes <- names(wand_fit$blueprint$ptypes$outcomes)[1]
  }
  outcomes <- paste0("Predicted ", outcomes)

  # Create nodes
  nodes <- data.frame(name = c(features, names(smooth_features), outcomes, "linear_layer"))

  # Create edges
  smooth_edges <- data.frame(
    from = unlist(smooth_features),
    to = rep(names(smooth_features), sapply(smooth_features, length))
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

  # TODO Should I include a plotting function? It's another dependency (ggraph) and introduces the
  # problem of...building plots that look nice.
  # ggraph(model_graph, layout = 'kk') +
  #   geom_edge_bend(aes(start_cap = label_rect(node1.name),
  #                          end_cap = label_rect(node2.name)),
  #                      arrow = arrow(length = unit(2, 'mm'))) +
  #   geom_node_label(aes(label = name))
}

#' Generates an evenly spaced sequence across a variable's values
#'
#' @param var A variable, numeric or factor.
#' @param seq_length An integer giving the maximum number of points in the sequence. If set to `inf`
#'   (the default) then the average spacing between points is used to determine sequence length. If
#'   `var` is non-numeric, then `seq_length` is ignored.
#'
#' @return An evenly spaced sequence from `var` min to max.
get_var_sequence <- function(var, seq_length = Inf) {
  if (is.numeric(var)) {
    # TODO this could be smarter, e.g. if there are only a few unique levels
    var <- sort(var)
    var_min <- min(var, na.rm = T)
    var_max <- max(var, na.rm = T)

    if (is.infinite(seq_length)) {
      var_spacing <- sapply(2:length(var), \(x) var[x] - var[x - 1])
      var_spacing <- mean(var_spacing[var_spacing > 0], na.rm = T)
      var_seq <- seq(from = var_min, to = var_max, by = var_spacing)
    } else {
      var_seq <- seq(from = var_min, to = var_max, length.out = seq_length)
    }
  } else if(is.factor(var)) {
    var_seq <- factor(levels(var), levels(var))
  } else{
    rlang::abort("`get_var_sequence` is not supported for argument `var`'s type.")
  }

  var_seq
}

#' @export
wand_plot_smooths <- function(wand_fit, df, seq_length = 250) {
  # Get features in each smooth
  smooth_features <- lapply(wand_fit$smooth_specs, \(x) names(x$blueprint$ptypes$predictors))
  outcome <- names(wand_fit$blueprint$ptypes$outcomes)[1]

  if (wand_fit$mode == "classification")
    prediction_mode <- "prob"
  else if (wand_fit$mode == "regression")
    prediction_mode <- "numeric"

  if (max(sapply(smooth_features, length)) > 2) {
    rlang::abort("Only smooths with a max of two features can currently be plotted.")
  }

  # plot each smooth
  smooth_plots <- list()
  for (smooth in smooth_features) {
    var_seq <- expand.grid(lapply(seq_along(smooth),
                                  # max of 1000 points per variable
                                  \(x) get_var_sequence(pull(df, smooth[x]), seq_length)))
    colnames(var_seq) <- smooth

    mean_df <- df %>%
      dplyr::select(-dplyr::all_of(c(smooth, outcome))) %>%
      # mean for numeric, mode for factor
      dplyr::summarise(dplyr::across(where(is.numeric), mean),
                       dplyr::across(where(is.factor), \(x) names(which.max(table(x)))[1]),
                       dplyr::across(where(is.character), \(x) names(which.max(table(x)))[1]))

    pdp_df <- cbind(mean_df, var_seq)

    pdp_df$.pred <- predict(wand_fit, new_data = pdp_df, type = prediction_mode)[[1]]

    if (length(smooth) == 1) {
      pl <- ggplot2::ggplot(pdp_df, ggplot2::aes_string(x = smooth[1], y = ".pred")) +
        ggplot2::geom_path()
    }
    else {
      pl <- ggplot2::ggplot(pdp_df, ggplot2::aes_string(x = smooth[1], y = smooth[2],
                                                        z = ".pred")) +
        ggplot2::stat_contour_filled() +
        ggplot2::labs(fill = ".pred")
    }

    smooth_plots[[length(smooth_plots) + 1]] <- pl
  }

  smooth_plots
}
