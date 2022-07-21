suppressPackageStartupMessages({
  library(dials)
  library(dplyr)
  library(modeldata)
  library(parsnip)
  library(recipes)
  library(rsample)
  library(tune)
})

# Prepare data
data(bivariate)
bivariate_train_folds <- vfold_cv(bivariate_train, v = 3)

test_that("`tunable.wand` has the expected parameters", {
  tunable_params <- tunable.wand("x")

  expect_true("batch_size" %in% tunable_params$name)
  expect_true("epochs" %in% tunable_params$name)
  expect_true("learn_rate" %in% tunable_params$name)
  expect_true("stop_iter" %in% tunable_params$name)
})

# batch_size
test_that("`nn_addtive_mod` has tunable `batch_size`", {
  bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
    step_log(all_numeric_predictors())

  wand_reg <- nn_additive_mod(mode = "regression",
                              batch_size = tune()) %>%
    set_engine("wand", smooth_specs = list(s_mlp(B)))

  expect_s3_class(
    tune_grid(wand_reg, bi_reg_rec, bivariate_train_folds,
              grid = tibble(batch_size = c(32, 64))),
    "tune_results"
  )
})


# epochs
test_that("`nn_addtive_mod` has tunable `epochs`", {
  bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
    step_log(all_numeric_predictors())

  wand_reg <- nn_additive_mod(mode = "regression",
                              epochs = tune()) %>%
    set_engine("wand", smooth_specs = list(s_mlp(B)))

  expect_s3_class(
    tune_grid(wand_reg, bi_reg_rec, bivariate_train_folds,
              grid = tibble(epochs = 1:3)),
    "tune_results"
  )
})

# learn_rate
test_that("`nn_addtive_mod` has tunable `learn_rate`", {
  bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
    step_log(all_numeric_predictors())

  wand_reg <- nn_additive_mod(mode = "regression",
                              learn_rate = tune()) %>%
    set_engine("wand", smooth_specs = list(s_mlp(B)))

  expect_s3_class(
    tune_grid(wand_reg, bi_reg_rec, bivariate_train_folds,
              grid = tibble(learn_rate = c(0.1, 0.01))),
    "tune_results"
  )
})

# stop_iter
test_that("`nn_addtive_mod` has tunable `stop_iter`", {
  bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
    step_log(all_numeric_predictors())

  wand_reg <- nn_additive_mod(mode = "regression",
                              stop_iter = tune()) %>%
    set_engine("wand", smooth_specs = list(s_mlp(B)))

  expect_s3_class(
    tune_grid(wand_reg, bi_reg_rec, bivariate_train_folds,
              grid = tibble(stop_iter = 1:3)),
    "tune_results"
  )
})

