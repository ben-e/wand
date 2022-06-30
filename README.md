# wand
{wand} is a forthcoming R package which implements wide and deep neural networks, a type of generalized additive model, with a focus on model interpretability and compatibility with the {tidymodels} ecosystem. {wand} draws heavy inspiration from the {mgcv} and {brulee} packages.

## Roadmap

### 0.1.0
First implementation of {wand}, featuring data frame, formula, matrix, and recipe interfaces. The formula interface draws inspiration from {mgcv}, allowing users to specify smooth terms and smooth interactions via an `s` function. The other interfaces allow interactions between terms to be set via function arguments.

- [ ] Implement base `wand` model fitting function.
- [ ] Implement the model constructor and bridge functions.
- [ ] Implement user facing interfaces.
- [ ] Implement prediction methods.
- [ ] Expand implementation to both regression and classification.
- [ ] Add support for feature interactions, similar to {mgcv}'s `ti` and `te`.

### 0.2.0
Improves {tidymodels} compatibility by adding {dial} tunable model parameters, including tuning for smooth model terms. This will allow users more tuning potential than the current {parsnip} implementation of `gen_additive_mod` built on {mgcv}.

- [ ] Add tunable parameters for model optimization.
- [ ] Add tunable parameters for specifying smooth parameters for each smooth.

### 0.3.0
Focuses on model explainability by adding `coef`, `summary`, and `plot` functions which mirror the functionality of the same funcitons in {mgcv}, as well as compatibility with the {ggeffects} and {marginaleffects} packages.

- [ ] Implement `coef`.
- [ ] Implement `summary`.
- [ ] Implement `plot`.
- [ ] PR for  {ggeffects} support.
- [ ] PR for  {marginaleffects} support.

### 0.4.0
Expands documentation, adding a project website and vignettes.

- [ ] Design hex.
- [ ] Build website.
- [ ] Write vignette: how does {wand} compare to {mgcv}.
- [ ] Write vignette: {wand} workflow guide.
