context("torch-modules")

test_that("the wand_mlp_module returns the correct number of layers", {
  skip_if(!torch::torch_is_installed())

  expect_equal({
    mod <- wand_mlp_module(1, c(32, 32))
    length(mod$modules$model$modules)
  }, 4)
})

test_that("the wand_mlp_module returns the correct number of output features", {
  skip_if(!torch::torch_is_installed())

  expect_equal({
    mod <- wand_mlp_module(1, c(73))
    length(mod$parameters[[length(mod$parameters)]])
  }, 73)
})

test_that("the wand_classification_module returns the correct number of classes", {
  skip_if(!torch::torch_is_installed())

  expect_equal({
    mod <- wand_classification_module(1, 7)
    length(mod$parameters[[length(mod$parameters)]])
  }, 7)
})
