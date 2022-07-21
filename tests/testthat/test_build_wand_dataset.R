# build_wand_dataset

test_that("`build_wand_dataset` can create a torch dataset", {
  expect_s3_class(
    build_wand_dataset(mtcars[ , -1], list(all = mtcars[ , -1]), mtcars[ , 1]),
    "tensor_dataset"
  )
})

test_that("`build_wand_dataset` can create a torch dataset with and without gradients", {
  expect_s3_class(
    build_wand_dataset(mtcars[ , -1], list(all = mtcars[ , -1]), mtcars[ , 1],
                       requires_grad = F),
    "tensor_dataset"
  )

  expect_s3_class(
    build_wand_dataset(mtcars[ , -1], list(all = mtcars[ , -1]), mtcars[ , 1],
                       requires_grad = T),
    "tensor_dataset"
  )
})

test_that("`build_wand_dataset` can create a torch dataset without smooth features", {
  expect_s3_class(
    build_wand_dataset(mtcars[ , -1], list(), mtcars[ , 1]),
    "tensor_dataset"
  )

  expect_error(
    build_wand_dataset(linear_predictors = mtcars[ , -1], outcome = mtcars[ , 1])
  )
})

test_that("`build_wand_dataset` can create a torch dataset without linear features", {
  expect_s3_class(
    build_wand_dataset(mtcars[ , -(1:ncol(mtcars))], list(all = mtcars[ , -1]), mtcars[ , 1]),
    "tensor_dataset"
  )

  expect_error(
    build_wand_dataset(smooth_features = list(all = mtcars[ , -1]), outcome = mtcars[ , 1])
  )
})

test_that("`build_wand_dataset` can create a torch dataset without an outcome", {
  expect_s3_class(
    build_wand_dataset(mtcars[ , -1], list(all = mtcars[ , -1])),
    "tensor_dataset"
  )
})
