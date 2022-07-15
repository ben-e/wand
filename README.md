# wand
{wand} is a forthcoming R package which implements wide and deep neural networks, a type of generalized additive model, with a focus on model interpretability and compatibility with the {tidymodels} ecosystem. {wand} draws heavy inspiration from the {mgcv} and {brulee} packages.

## Roadmap

### 0.0.1
First implementation of {wand}, featuring data frame, formula, matrix, and recipe interfaces. The formula interface draws inspiration from {mgcv}, allowing users to specify smooth terms and smooth interactions via an `s` function. The other interfaces allow interactions between terms to be set via function arguments.

- [x] Implement base `wand` model fitting function.
- [x] Implement the model constructor and bridge functions.
- [x] Implement user facing interfaces.
- [x] Implement prediction methods.
- [x] Expand implementation to both regression and classification.
- [x] Add support for feature interactions, similar to {mgcv}'s `ti` and `te`.
- [ ] Add tests for each user interface
- [x] Document functions and add examples.

### 0.0.2
Improves {tidymodels} compatibility by adding {dial} tunable model parameters, including tuning for smooth model terms. This will allow users more tuning potential than the current {parsnip} implementation of `gen_additive_mod` built on {mgcv}.

- [ ] Add tunable parameters for model optimization.
- [ ] Add tunable parameters for output layer regularization.
- [ ] Allow specified models to be "compiled" such that parameters for all smooths are tunable in the {tidymodels} framework, similar to the [skorch](https://github.com/skorch-dev/skorch) Python package's interface between PyTorch and scikit-learn.

### 0.0.3
Focuses on model explainability by adding `coef`, `summary`, and `plot` functions which mirror the functionality of the same funcitons in {mgcv}, as well as compatibility with the {ggeffects} and {marginaleffects} packages.

- [ ] Implement `coef`.
- [ ] Implement `summary`.
- [ ] Implement `plot`.
- [ ] PR for  {ggeffects} support.
- [ ] PR for  {marginaleffects} support.

### 0.0.4
Expands documentation, adding a project website and vignettes.

- [ ] Design hex.
- [ ] Build website.
- [ ] Write vignette: how does {wand} compare to {mgcv}.
- [ ] Write vignette: {wand} workflow guide.

## Related Work

The ideas underpinning this package are not wholly original, in particular this package draws from:

* The original [Wide & Deep Learning for Recommender Systems](https://arxiv.org/abs/1606.07792) paper.
* The formula interface and model specification options available in {mgcv}.
* The interface between the {torch} package and {tidymodels} used by the {brulee} package.

During the creation of this package, I have also stumbled on a few related packages:

* The [PySDDR](https://github.com/HelmholtzAI-Consultants-Munich/PySDDR) also implements the wide and deep idea, but provides a comprehensive interface capable of implementing many types of deep models and can train distributional regressions (e.g. Poisson regression).
