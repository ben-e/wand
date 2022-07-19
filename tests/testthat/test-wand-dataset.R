test_that("build_wand_dataset builds a dataset", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    build_wand_dataset(mtcars[ , -1], mtcars[ , 1], smooth_specs = list(hp = s_mlp(hp)))
    T
  })
})

test_that("build_wand_dataset builds a dataset with missing smooth features", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    build_wand_dataset(mtcars[ , -1], mtcars[ , 1])
    T
  })
})

test_that("build_wand_dataset builds a dataset with empty smooth features", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    build_wand_dataset(mtcars[ , -1], mtcars[ , 1], smooth_specs = list())
    T
  })
})

test_that("build_wand_dataset builds a dataset with no linear features", {
  skip_if(!torch::torch_is_installed())

  expect_true({
    build_wand_dataset(mtcars[ , "hp", drop = F], mtcars[ , 1],
                       smooth_specs = list(hp = s_mlp(hp)))
    T
  })
})


