# dehydrate_model and hydrate_model
test_that("`torch` objects can be dehydrated and rehydrated", {
  expect_type({
    dehydrate_model(torch::torch_tensor(1))
  },
  "raw"
  )

  expect_s3_class({
    hydrate_model(dehydrate_model(torch::torch_tensor(1)))
  },
  "torch_tensor"
  )

  expect_type({
    dehydrate_model(wand_mlp_module(1, 1))
  },
  "raw"
  )

  expect_s3_class({
    hydrate_model(dehydrate_model(wand_mlp_module(1, 1)))
  },
  "wand_mlp_module"
  )
})

# scale_y
test_that("numeric vectors can be scaled", {
  expect_equal(scale_y(as.double(1), list(mean = 0, sd = 1)), 1)

  expect_equal(scale_y(as.integer(1), list(mean = 0, sd = 1)), 1)

  expect_equal(scale_y(1, list(mean = 0, sd = 0.5)), 2)

  expect_equal(scale_y(1:5, list(mean = 0, sd = 0.5)), 2*1:5)

  expect_equal(scale_y(1:5, list(mean = 1, sd = 0.5)), 2*(1:5 - 1))

  expect_error(scale_y(as.factor(1:5), list(mean = 0, sd = 1)))
})

# validate_smooth_formula
test_that("formulas for smooths can be validated", {
  expect_equal(validate_smooth_formula( ~ x), ~ x)

  expect_error(validate_smooth_formula(y ~ x))
})

test_that("formulas for smooths can be validated", {
  expect_true(all.equal(dots_to_formula(x, y, z), ~ x + y + z))

  expect_error(dots_to_formula())
})

# extract_smooths
test_that("smooth specifications can be extracted from formulas", {
  expect_true(all.equal(extract_smooths(y ~ x + s_mlp(a))$formula, y ~ x + a))

  expect_equal(length(extract_smooths(y ~ x + s_mlp(a, b) + s_mlp(b))$smooth_specs), 2)

  expect_equal(length(extract_smooths(y ~ x)$smooth_specs), 0)
})


test_that("`spacing` returns the correct spacing", {
  expect_equal(spacing(1:10), 1)

  expect_equal(spacing(-10:10), 1)

  expect_equal(spacing(seq(0, 5, 0.25)), 0.25)

  expect_error(spacing(letters))
})

test_that("`typical` returns typical values", {
  expect_equal(typical(1:3), 2)

  expect_equal(typical(as.integer(1:3)), 2)

  expect_equal(as.character(typical(as.factor(c(letters, letters[2])))), "b")
  expect_equal(as.character(typical(ordered(c(letters[1:2], letters[2]),
                                            levels = letters[1:2]))),
               "b")

  expect_equal(typical(c(letters, letters[2])), "b")

  expect_true(typical(c(T, T, F)))
  expect_false(typical(c(F, F, T, T, F)))

})
