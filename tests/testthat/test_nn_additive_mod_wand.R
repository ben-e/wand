suppressPackageStartupMessages({
  library(dplyr)
  library(modeldata)
  library(parsnip)
  library(recipes)
  library(workflows)
})

# Prepare data
data(bivariate)

test_that("`nn_additive_mod` classification and regression models can be specified", {
  expect_s3_class({
    bi_class_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())

    wand_class <- nn_additive_mod(mode = "classification") %>%
      set_engine("wand", smooth_specs = list(s_mlp(A)))

    bi_class_wf <- workflow() %>%
      add_recipe(bi_class_rec) %>%
      add_model(wand_class)

    bi_class_wf
  },
  "workflow"
  )

  expect_s3_class({
    bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())

    wand_reg <- nn_additive_mod(mode = "regression") %>%
      set_engine("wand", smooth_specs = list(s_mlp(B)))

    bi_reg_wf <- workflow() %>%
      add_recipe(bi_reg_rec) %>%
      add_model(wand_reg)

    bi_reg_wf
  },
  "workflow"
  )
})

test_that("`nn_additive_mod` classification and regression models can be fit", {
  expect_s3_class({
    bi_class_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())

    wand_class <- nn_additive_mod(mode = "classification") %>%
      set_engine("wand", smooth_specs = list(s_mlp(A)))

    bi_class_wf <- workflow() %>%
      add_recipe(bi_class_rec) %>%
      add_model(wand_class)

    bi_class_fit <- fit(bi_class_wf, bivariate_train)
    bi_class_fit
  },
  "workflow"
  )

  expect_s3_class({
    bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())
    wand_reg <- nn_additive_mod(mode = "regression") %>%
      set_engine("wand", smooth_specs = list(s_mlp(B)))
    bi_reg_wf <- workflow() %>%
      add_recipe(bi_reg_rec) %>%
      add_model(wand_reg)

    bi_reg_fit <- fit(bi_reg_wf, bivariate_train)
    bi_reg_fit
  },
  "workflow"
  )
})

test_that("fitted `nn_additive_mod` classification and regression models can predict", {
  expect_s3_class({
    bi_class_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())

    wand_class <- nn_additive_mod(mode = "classification") %>%
      set_engine("wand", smooth_specs = list(s_mlp(A)))

    bi_class_wf <- workflow() %>%
      add_recipe(bi_class_rec) %>%
      add_model(wand_class)

    bi_class_fit <- fit(bi_class_wf, bivariate_train)

    predict(bi_class_fit, bivariate_test)[[1]]
  },
  "factor"
  )

  expect_type({
    bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
      step_log(all_numeric_predictors())

    wand_reg <- nn_additive_mod(mode = "regression") %>%
      set_engine("wand", smooth_specs = list(s_mlp(B)))

    bi_reg_wf <- workflow() %>%
      add_recipe(bi_reg_rec) %>%
      add_model(wand_reg)

    bi_reg_fit <- fit(bi_reg_wf, bivariate_train)

    predict(bi_reg_fit, bivariate_test)[[1]]
  },
  "double"
  )
})

# TODO How do I test `add_nn_additive_mod`?


