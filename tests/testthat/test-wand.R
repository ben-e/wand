context("wand")

suppressPackageStartupMessages(library(recipes))

test_that("wand.data.frame fits without error", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    mod <- wand(mtcars[ , -1], mtcars[ , 1], smooth_specs = list(hp = s_mlp(hp)))
    T
  })
})

test_that("wand.formula fits without error", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    mod <- wand(mpg ~ disp + s_mlp(hp), data = mtcars)
    T
  })
})

test_that("wand.recipe fits without error", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    rec <- recipe(mpg ~ ., mtcars)
    rec <- step_log(rec, disp)
    mod <- wand(rec, mtcars, smooth_specs = list(hp = s_mlp(hp)))
    T
  })
})

test_that("wand stops early with no improvement", {
  skip_if(!torch::torch_is_installed())

  expect_length({
    mod <- wand(mtcars[ , -1], mtcars[ , 1], smooth_specs = list(hp = s_mlp(hp)),
                learn_rate = 0, stop_iter = 5)
    na.omit(mod$loss)
  },
  6)
})

test_that("wand numeric predictions are numeric", {
  skip_if(!torch::torch_is_installed())

  expect_type({
    mod <- wand(mtcars[ , -1], mtcars[ , 1], smooth_specs = list(hp = s_mlp(hp)))
    predict(mod, mtcars[ , -1], type = "numeric")$.pred
  },
  "double")
})

test_that("wand probability predictions are probabilities", {
  skip_if(!torch::torch_is_installed())

  mod <- wand(mtcars[ , -11], as.factor(mtcars[ , 11]), smooth_specs = list(hp = s_mlp(hp)))

  expect_lte({max(predict(mod, mtcars[ , -11], type = "prob")$.pred_1)}, 1)
  expect_gte({min(predict(mod, mtcars[ , -11], type = "prob")$.pred_1)}, 0)
})

test_that("wand class predictions are factors", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    mod <- wand(mtcars[ , -11], as.factor(mtcars[ , 11]), smooth_specs = list(hp = s_mlp(hp)))
    is.factor(predict(mod, mtcars[ , -11])$.pred_class)
  })
})
