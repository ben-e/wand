suppressPackageStartupMessages({
  library(dplyr)
  library(modeldata)
  library(recipes)
})

# Prepare data
data(bivariate)

# Classification
bi_class_smooth_fmla <- Class ~ s_mlp(log(A)) + log(B)
# Recipes have a slightly different prediction method than other approaches
bi_class_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
  step_log(all_numeric_predictors())

# Regression
bi_reg_smooth_fmla <- A ~ s_mlp(log(B))
bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
  step_log(all_numeric_predictors())

test_that("wand numeric predictions are numeric", {
  expect_type({
    mod <- wand(bi_reg_smooth_fmla, data = bivariate_train)
    predict(mod, bivariate_test, type = "numeric")$.pred
  },
  "double"
  )

  # make sure that numeric is the default for regression mode
  expect_type({
    mod <- wand(bi_reg_smooth_fmla, data = bivariate_train)
    predict(mod, bivariate_test)$.pred
  },
  "double"
  )

  expect_type({
    mod <- wand(bi_reg_rec, data = bivariate_train)
    predict(mod, bivariate_test)$.pred
  },
  "double"
  )

  expect_type({
    mod <- wand(bi_reg_rec, data = bivariate_train, smooth_specs = list(s_mlp(B)))
    predict(mod, bivariate_test)$.pred
  },
  "double"
  )
})

test_that("wand probability predictions are probabilities", {
  mod <- wand(bi_class_smooth_fmla, data = bivariate_train)

  expect_lte(max(predict(mod, bivariate_test, type = "prob")$pred_One), 1)
  expect_gte(min(predict(mod, bivariate_test, type = "prob")$pred_One), 0)
})

test_that("wand class predictions are factors", {
  mod <- wand(bi_class_smooth_fmla, data = bivariate_train)
  expect_s3_class(predict(mod, bivariate_test, type = "class")[[1]], "factor")
  # make sure class is the default for factor outcomes
  expect_s3_class(predict(mod, bivariate_test)[[1]], "factor")

  expect_s3_class({
    mod <- wand(bi_class_rec, data = bivariate_train)
    predict(mod, bivariate_test)$.pred_class
  },
  "factor"
  )

  expect_s3_class({
    mod <- wand(bi_class_rec, data = bivariate_train, smooth_specs = list(s_mlp(B)))
    predict(mod, bivariate_test)$.pred_class
  },
  "factor"
  )
})
