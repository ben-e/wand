suppressPackageStartupMessages({
  library(dplyr)
  library(modeldata)
  library(recipes)
})

# Prepare data
data(bivariate)

# Prepare formulas, recipes

# Classification
bi_class_fmla <- Class ~ log(A) + log(B)
bi_class_rec <- recipe(Class ~ A + B, data = bivariate_train) %>%
  step_log(all_numeric_predictors())
bi_class_x <- log(bivariate_train[ , 1:2, drop = F])
bi_class_y <- bivariate_train[ , 3, drop = F]

# Classification with smooths
bi_class_smooth_fmla <- Class ~ s_mlp(log(A)) + log(B)
# TODO Other interfaces with smooths?

# Regression
bi_reg_fmla <- A ~ log(B)
bi_reg_rec <- recipe(A ~ B, data = bivariate_train) %>%
  step_log(all_numeric_predictors())
bi_reg_x <- log(bivariate_train[ , 2, drop = F])
bi_reg_y <- bivariate_train[ , 1, drop = F]

# Regression with smooths
bi_reg_smooth_fmla <- A ~ s_mlp(log(B))
# TODO Other interfaces with smooths?

test_that("all `wand` interfaces work as expected", {
  # default
  expect_error(wand(as.vector(bivariate_train[, 1]), as.vector(bivariate_train[, 3])))
  expect_error(wand(as.vector(bivariate_train[, 1]), as.vector(bivariate_train[, 2])))

  # data.frame x/y
  expect_s3_class(wand(bi_class_x, bi_class_y), "wand")
  expect_s3_class(wand(bi_reg_x, bi_reg_y), "wand")

  # matrix x/y
  expect_s3_class(wand(as.matrix(bi_class_x), bi_class_y[[1]]), "wand")
  expect_s3_class(wand(as.matrix(bi_reg_x), bi_reg_y[[1]]), "wand")
  expect_error({
    bi_class_x_no_names <- as.matrix(bi_class_x)
    colnames(bi_class_x_no_names) <- NULL
    wand(bi_class_x_no_names, bi_class_y[[1]])
  })

  # formula
  expect_s3_class(wand(bi_class_fmla, bivariate_train), "wand")
  expect_s3_class(wand(bi_class_smooth_fmla, bivariate_train), "wand")
  expect_s3_class(wand(bi_reg_fmla, bivariate_train), "wand")
  expect_s3_class(wand(bi_reg_smooth_fmla, bivariate_train), "wand")

  # recipe
  expect_s3_class(wand(bi_class_rec, bivariate_train), "wand")
  expect_s3_class(wand(bi_reg_rec, bivariate_train), "wand")
})

test_that("wand fits with integer and double outcomes", {
  expect_s3_class(wand(as.matrix(bi_reg_x), as.integer(bi_reg_y[[1]])), "wand")
  expect_s3_class(wand(as.matrix(bi_reg_x), as.double(bi_reg_y[[1]])), "wand")
})

test_that("`wand` stops early when no improvement happens", {
  expect_length(
    na.omit(
      wand(bi_reg_rec, bivariate_train,
           learn_rate = 0, stop_iter = 5, validation_prop = 0)$training_results$loss
      ),
    6
  )

  expect_length(
    na.omit(
      wand(bi_reg_rec, bivariate_train,
           learn_rate = 0, stop_iter = 5, validation_prop = 0.5)$training_results$loss
    ),
    6
  )
})

test_that("`wand` fits with and without a validation set", {
  expect_s3_class(wand(bi_class_fmla, bivariate_train, validation_prop = 0), "wand")
  expect_s3_class(wand(bi_class_fmla, bivariate_train, validation_prop = 0.5), "wand")

  expect_s3_class(wand(bi_reg_fmla, bivariate_train, validation_prop = 0), "wand")
  expect_s3_class(wand(bi_reg_fmla, bivariate_train, validation_prop = 0.5), "wand")
})

test_that("`wand` can be verbose", {
  expect_message(
    wand(bi_class_fmla, bivariate_train, verbose = T),
    "^mode"
  )
})
