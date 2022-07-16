context("torch-serialization")

test_that("model hydration followed by dehydration returns a raw object", {
  expect_type({
    mod <- wand(mtcars[, -1], mtcars[, 1], smooth_specs = list(hp = s_mlp(hp)))
    dehydrate_model(hydrate_model(mod$model_obj))
  },
  "raw")
})
