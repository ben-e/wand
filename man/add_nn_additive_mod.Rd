% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nn_additive_mod_wand.R
\name{add_nn_additive_mod}
\alias{add_nn_additive_mod}
\title{Add \code{nn_additive_mod} as a model spec and \code{wand} as engine to parsnip}
\usage{
add_nn_additive_mod()
}
\description{
Add \code{nn_additive_mod} as a model spec and \code{wand} as engine to parsnip
}
\note{
Ideally, \code{wand} should fall under \code{gen_additve_mod}, but I've had issues working
around the assumptions that parsnip makes about this model spec. I will try to incorporate
\code{wand} with \code{nn_additive_mod} in the future.
}
