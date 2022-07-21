
<!-- README.md is generated from README.Rmd. Please edit that file -->

# wand

<!-- badges: start -->

[![R-CMD-check](https://github.com/ben-e/wand/workflows/R-CMD-check/badge.svg)](https://github.com/ben-e/wand/actions)
[![Codecov test
coverage](https://codecov.io/gh/ben-e/wand/branch/main/graph/badge.svg)](https://app.codecov.io/gh/ben-e/wand?branch=main)
<!-- badges: end -->

`wand` is an R package which implements generalized additive models
(GAMs) using `torch` as a backend, allowing users to fit
semi-interpretable models using tools from the deep learning world. In
addition to its GAM implementation, `wand` also provides tools for model
interpretation. This package gets its name from the type of network it
implements: **w**ide **an**d **d**eep networks combine linear features
(wide) with feature embeddings (deep).

`wand` draws heavy inspiration from the `mgcv` and `brulee` packages.

## Installation

The development version of can be installed from
[GitHub](https://github.com/ben-e/wand) with:

``` r
# install.packages("pak")
pak::pak("ben-e/wand")
```

Note that `wand` is still in active development, and should not be
considered stable.

## Example

The primary model fitting function, `wand::wand` has formula, x/y, and
recipe user interfaces, and compatibility with the `tidymodels`
ecosystem via the `nn_additive_model` specification and `wand` engine.

Using `wand` alone:

``` r
suppressPackageStartupMessages({
  library(wand)
  library(dplyr)
  library(parsnip)
  library(recipes)
  library(workflows)
  library(yardstick)
})

data(bivariate, package = "modeldata")

wand_fit <- wand(Class ~ log(A) + s_mlp(log(B), hidden_units = c(16, 16, 8)),
                 data = bivariate_train)
predict(wand_fit, bivariate_test, type = "prob") %>% 
  bind_cols(bivariate_test) %>% 
  roc_auc(Class, .pred_Two)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.744
```

Using `wand` with the `tidymodels` ecosystem:

``` r
wand_recipe <- recipe(Class ~ A + B, 
                      data = bivariate_train) %>% 
  step_log(all_numeric_predictors())

wand_model_spec <- nn_additive_mod(mode = "classification") %>% 
  set_engine("wand", smooth_specs = list(B = s_mlp(B, hidden_units = c(16, 16, 8))))
# note that `B` in this smooth will already have been log transformed

wand_wf <- workflow() %>% 
  add_recipe(wand_recipe) %>% 
  add_model(wand_model_spec)

wand_wf_fit <- fit(wand_wf, bivariate_train)

predict(wand_wf_fit, bivariate_test, type = "prob") %>% 
  bind_cols(bivariate_test) %>% 
  roc_auc(Class, .pred_Two)
#> # A tibble: 1 × 3
#>   .metric .estimator .estimate
#>   <chr>   <chr>          <dbl>
#> 1 roc_auc binary         0.751
```

## Feature Roadmap

### Models

-   [x] Implement `linear` regression and classification modules with
    `torch`.
-   [x] Implement multilayer perceptron module with `torch`.
-   [x] Implement wide and deep module with `torch`.
-   [ ] Explore implementation of other deep module architectures (RNN,
    CNN, transformer) with a focus on fitting mixed-type data
    (e.g. tabular + image data).
-   [ ] Add L1/L2 penalties to optimization, see `brulee`.
-   [ ] Explore use of MC dropout to generate prediction intervals.

### User Interface

-   [x] Implement formula, x/y, and recipe user interfaces via
    `hardhat`.
-   [x] Add `parsnip` and `workflows` compatibility.
-   [x] Add specification functions for smooths, similar to `mgcv::s`,
    for all interfaces.
-   [ ] Explore the use of constructor functions for new smooth
    specification functions, allowing users to specify their own
    smoothers using their own `torch::nn_module`’s.
-   [ ] Explore the use of a smooth function registry, similar to how
    `parsnip` tracks models.

### Tuning

-   [ ] Add `tune` compatibility for model training parameters.
-   [ ] Add `tune` compatibility for output layer regularization,
    similar to `brulee`.
-   [ ] Explore possibility of “compiling” models such that parameters
    for all smooths are `tune` compatible, similar to the
    [`skorch`](https://github.com/skorch-dev/skorch) Python package’s
    bridge between PyTorch and scikit-learn.

### Interpretability Tools

-   [ ] Implement `coef` method to extract linear coefficients.
-   [ ] Implement `summary` method.
-   [ ] Implement `plot` methods for plotting training curves and smooth
    functions (ala `mgcv`).
-   [ ] Implement graph/module plot to show how features flow through
    modules.
-   [ ] Add compatibility with the `marginaleffects` package.
-   [ ] Add compatibility with the `ggeffects` package.

### Documentation

-   [ ] Design hex.
-   [ ] Build `pkgdown` website.
-   [ ] Write vignette: how does `wand` compare to `mgcv`.
-   [ ] Write vignette: `wand` workflow guide.
-   [ ] Write vignette: a deep dive into wide and deep neural networks.
-   [x] Document functions and add examples.
-   [x] Add tests for each user interface.
-   [x] Add tests for internal functions.
-   [ ] **Ongoing** Add more tests :)

## Related Work

The ideas underpinning this package are not original, in particular this
package draws from:

-   The original [Wide & Deep Learning for Recommender
    Systems](https://arxiv.org/abs/1606.07792) paper.
-   The formula interface and model specification options available in
    {mgcv}.
-   The interface between the {torch} package and {tidymodels} used by
    the {brulee} package.

`wand` is, of course, not the only implementation of wide and deep
networks:

-   [`PySDDR`](https://github.com/HelmholtzAI-Consultants-Munich/PySDDR)
-   [`tf.keras.experimental.WideDeepModel`](https://www.tensorflow.org/api_docs/python/tf/keras/experimental/WideDeepModel)
