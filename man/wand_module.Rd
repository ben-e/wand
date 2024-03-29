% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/torch_modules.R
\name{wand_module}
\alias{wand_module}
\title{A \code{torch::nn_module} module used to fit wide and deep neural networks}
\usage{
wand_module(n_linear_features, smooth_specs, mode, n_classes)
}
\arguments{
\item{n_linear_features}{An integer giving the number of linear features.}

\item{smooth_specs}{A named list of smooth specifications, e.g. \code{s_mlp}.}

\item{mode}{A string giving the model's goal: "classification" or "regression".}

\item{n_classes}{An integer giving the number of classes, required when
\code{mode == "classification"}.}
}
\description{
This module is not meant to be used directly by users. It makes many assumptions about inputs,
and is used as the main engine of the \code{wand} package.
}
