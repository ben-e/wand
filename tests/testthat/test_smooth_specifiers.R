test_that("`s_mlp` returns the correct number of features", {
  expect_error(s_mlp())

  expect_equal(length(s_mlp(a, b, c)$features), 3)

  # TODO expressions can not be evaluated
  # This will throw a downstream error, but not at s_mlp
  # expect_error(s_mlp((a + b), a, b, c))
})

test_that("`s_mlp` returns a `torch` module that can be initialized", {
  expect_s3_class({
    x <- s_mlp(a, b, c)
    do.call(x$torch_module, x$torch_module_parameters)
  }, "nn_module")

  expect_s3_class({
    x <- s_mlp(a, b, c)
    do.call(x$torch_module, x$torch_module_parameters)
  }, "wand_mlp_module")

  expect_equal({
    x <- s_mlp(a)
    do.call(x$torch_module, x$torch_module_parameters)$model[[1]]$weight$shape[2]
  }, 1)

  expect_equal({
    x <- s_mlp(a, b, c)
    do.call(x$torch_module, x$torch_module_parameters)$model[[1]]$weight$shape[2]
  }, 3)
})
