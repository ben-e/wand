
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
# TODO
```

Using `wand` with the `tidymodels` ecosystem:

``` r
# TODO
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

### User Interface

-   [x] Implement formula, x/y, and recipe user interfaces via
    `hardhat`.
-   [x] Add `parsnip` and `workflows` compatibility.
-   [x] Add specification functions for smooths, similar to `mgcv::s`,
    for all interfaces.
-   [ ] Explore the use of constructor functions for new smooth
    specification functions, allowing users to specify their own
    smoothers using their own `torch::nn_module`’s.

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
-   **Ongoing** Add more tests :)

## Related Work

The ideas underpinning this package are not wholly original, in
particular this package draws from:

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
