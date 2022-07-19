suppressPackageStartupMessages({
  library(parsnip)
  library(recipes)
  library(workflows)
})

# TODO how do I test wand::add_nn_additive_mod?

test_that("`nn_additive_mod` fits regression and classification models", {
  # regression
  expect_true({
    mtcars_rec <- recipe(mpg ~ hp,
                           data = mtcars) %>%
      step_normalize(all_numeric_predictors()) %>%
      step_naomit(everything()) %>%
      step_shuffle()

    wand_spec <- nn_additive_mod(mode = "regression") %>%
      set_engine("wand", smooth_specs = list(s_mlp(hp)))

    wand_wf <- workflow() %>%
      add_recipe(mtcars_rec) %>%
      add_model(wand_spec)

    wand_fit <- fit(wand_wf, data = mtcars)

    T
  })

  # classification
  expect_true({
    mtcars$vs <- factor(mtcars$vs, 0:1)
    mtcars_rec <- recipe(vs ~ hp,
                         data = mtcars) %>%
      step_normalize(all_numeric_predictors()) %>%
      step_naomit(everything()) %>%
      step_shuffle()

    wand_spec <- nn_additive_mod(mode = "classification") %>%
      set_engine("wand", smooth_specs = list(s_mlp(hp)))

    wand_wf <- workflow() %>%
      add_recipe(mtcars_rec) %>%
      add_model(wand_spec)

    wand_fit <- fit(wand_wf, data = mtcars)

    T
  })
})
