# wand_mlp_module
test_that("`wand_mlp_module` builds the correct number of layers", {
  # These divide by 2 to account for activation layers.
  # For hidden layers > 1, add 1 to account for the final layer omitting the activqation.

  expect_equal(
    length(wand_mlp_module(1, 0)$model)/2,
    1
  )

  expect_equal(
    length(wand_mlp_module(1, 1)$model)/2,
    1
  )

  expect_equal(
    (length(wand_mlp_module(1, rep(1, 2))$model) + 1)/2,
    2
  )

  expect_equal(
    (length(wand_mlp_module(1, rep(1, 3))$model) + 1)/2,
    3
  )

  expect_equal(
    (length(wand_mlp_module(1, rep(1, 4))$model) + 1)/2,
    4
  )
})

test_that("`wand_mlp_module` takes the `n_features`", {
  expect_error(wand_mlp_module(0, 0))

  expect_equal(
    wand_mlp_module(1, 0)$model[[1]]$weight$shape[2],
    1
  )

  expect_equal(
    wand_mlp_module(1, 1)$model[[1]]$weight$shape[2],
    1
  )

  expect_equal(
    wand_mlp_module(10, 1:10)$model[[1]]$weight$shape[2],
    10
  )
})

# wand_regression_module
test_that("`wand_regression_module` takes `n_features` and returns 1 output", {
  expect_error(wand_regression_module(0))

  expect_equal(
    wand_regression_module(1)$model$weight$shape[2],
    1
  )

  expect_equal(
    wand_regression_module(5)$model$weight$shape[2],
    5
  )

  expect_equal(
    wand_regression_module(1)$model$weight$shape[1],
    1
  )

  expect_equal(
    wand_regression_module(5)$model$weight$shape[1],
    1
  )
})

# wand_classification_module
test_that("`wand_classification_module` takes the `n_features` and returns `n_classes`", {
  expect_error(wand_classification_module(0, 0))

  expect_error(wand_classification_module(1, 1))

  expect_equal(
    wand_classification_module(1, 2)$model[[1]]$weight$shape[2],
    1
  )

  expect_equal(
    wand_classification_module(5, 2)$model[[1]]$weight$shape[2],
    5
  )

  expect_equal(
    wand_classification_module(1, 2)$model[[1]]$weight$shape[1],
    2
  )

  expect_equal(
    wand_classification_module(5, 3)$model[[1]]$weight$shape[1],
    3
  )
})

# wand_module
test_that("`wand_module` can be initiated", {
  expect_error(wand_module(0, list(), "regression", 0))
  expect_error(wand_module(0, list(), "classification", 0))
  expect_error(wand_module(0, list(), "classification", 2))

  expect_true("smooth_modules" %in%
                names(wand_module(2, list(x = s_mlp(x)), "regression")$modules))

  expect_true("linear_module" %in%
                names(wand_module(2, list(x = s_mlp(x)), "regression")$modules))

  expect_true("smooth_modules" %in%
                names(wand_module(2, list(x = s_mlp(x)), "classification", 2)$modules))

  expect_true("linear_module" %in%
                names(wand_module(2, list(x = s_mlp(x)), "classification", 2)$modules))

  expect_s3_class(wand_module(2, list(x = s_mlp(x)), "regression"), "wand_module")
  expect_s3_class(wand_module(2, list(x = s_mlp(x)), "regression"), "nn_module")
  expect_s3_class(wand_module(2, list(x = s_mlp(x)), "classification", 2), "wand_module")
  expect_s3_class(wand_module(2, list(x = s_mlp(x)), "classification", 2), "nn_module")
})

test_that("`wand_module` can be built without linear features", {
  expect_true("smooth_modules" %in%
                names(wand_module(0, list(x = s_mlp(x)), "regression")$modules))

  expect_true("linear_module" %in%
                names(wand_module(0, list(x = s_mlp(x)), "regression")$modules))

  expect_true("smooth_modules" %in%
                names(wand_module(0, list(x = s_mlp(x)), "classification", 2)$modules))

  expect_true("linear_module" %in%
                names(wand_module(0, list(x = s_mlp(x)), "classification", 2)$modules))

  expect_s3_class(wand_module(0, list(x = s_mlp(x)), "regression"), "wand_module")
  expect_s3_class(wand_module(0, list(x = s_mlp(x)), "regression"), "nn_module")
  expect_s3_class(wand_module(0, list(x = s_mlp(x)), "classification", 2), "wand_module")
  expect_s3_class(wand_module(0, list(x = s_mlp(x)), "classification", 2), "nn_module")
})


test_that("`wand_module` can be built without smooth features", {
  expect_true("smooth_modules" %in%
                names(wand_module(2, list(), "regression")$modules))

  expect_true("linear_module" %in%
                names(wand_module(2, list(), "regression")$modules))

  expect_true("smooth_modules" %in%
                names(wand_module(2, list(), "classification", 2)$modules))

  expect_true("linear_module" %in%
                names(wand_module(2, list(), "classification", 2)$modules))

  expect_s3_class(wand_module(2, list(), "regression"), "wand_module")
  expect_s3_class(wand_module(2, list(), "regression"), "nn_module")
  expect_s3_class(wand_module(2, list(), "classification", 2), "wand_module")
  expect_s3_class(wand_module(2, list(), "classification", 2), "nn_module")
})
