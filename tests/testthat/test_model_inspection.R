mtcars$cyl <- as.factor(mtcars$cyl)

wand_fit_reg <- wand(mpg ~ drat + s_mlp(disp) + s_mlp(hp, wt), data = mtcars)
wand_fit_reg_no_smooth <- wand(mpg ~ hp + wt, data = mtcars)
wand_fit_reg_all_smooth <- wand(mpg ~ s_mlp(hp), data = mtcars)
wand_fit_reg_interact <- wand(mpg ~ drat + s_mlp(hp, wt, disp), data = mtcars)
wand_fit_reg_early <- wand(mpg ~ drat + s_mlp(hp, wt),
                           learn_rate = 10, stop_iter = 1,
                           data = mtcars)
wand_fit_class <- wand(cyl ~ mpg + s_mlp(disp) + s_mlp(hp, wt), data = mtcars)


test_that("`wand_plot_loss` returns a plot", {
  expect_s3_class(wand_plot_loss(wand_fit_reg), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_reg_early), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_class), "ggplot")

  expect_s3_class(wand_plot_loss(wand_fit_reg, show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_reg_early, show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_class, show_labels = F), "ggplot")

  expect_s3_class(wand_plot_loss(wand_fit_reg, show_best_epoch = F, show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_reg_early, show_best_epoch = F, show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_class, show_best_epoch = F, show_labels = F), "ggplot")

  expect_s3_class(wand_plot_loss(wand_fit_reg, show_best_epoch = F, show_early_stopping = F,
                                 show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_reg_early, show_best_epoch = F, show_early_stopping = F,
                                 show_labels = F), "ggplot")
  expect_s3_class(wand_plot_loss(wand_fit_class, show_best_epoch = F, show_early_stopping = F,
                                 show_labels = F), "ggplot")
})

test_that("`wand_plot_smooths` returns the valid plots", {
  expect_s3_class(wand_plot_smooths(wand_fit_reg)[[1]], "ggplot")
  expect_s3_class(wand_plot_smooths(wand_fit_reg_early)[[1]], "ggplot")
  expect_s3_class(wand_plot_smooths(wand_fit_class)[[1]], "ggplot")

  expect_s3_class(wand_plot_smooths(wand_fit_reg)[[2]], "ggplot")
  expect_s3_class(wand_plot_smooths(wand_fit_class)[[2]], "ggplot")

  expect_s3_class(wand_plot_smooths(wand_fit_reg, seq_length = 1)[[1]], "ggplot")
  expect_s3_class(wand_plot_smooths(wand_fit_class, seq_length = 1)[[1]], "ggplot")

  expect_s3_class(wand_plot_smooths(wand_fit_reg, seq_length = 100)[[1]], "ggplot")
  expect_s3_class(wand_plot_smooths(wand_fit_class, seq_length = 100)[[1]], "ggplot")

  expect_error(wand_plot_smooths(wand_fit_reg_interact))
})

test_that("`wand_plot_smooths` returns the correct number of plots", {
  expect_length(wand_plot_smooths(wand_fit_reg), 2)
  expect_length(wand_plot_smooths(wand_fit_class), 2)
})

test_that("`build_wand_graph` returns valid graphs", {
  expect_s3_class(build_wand_graph(wand_fit_reg), "igraph")
  expect_s3_class(build_wand_graph(wand_fit_class), "igraph")
})

test_that("`coef.wand` returns model coefficients", {
  expect_equal(nrow(coef(wand_fit_reg)), 2)
  expect_equal(nrow(coef(wand_fit_reg_no_smooth)), 3)
  expect_equal(nrow(coef(wand_fit_reg_all_smooth)), 1)
  expect_equal(nrow(coef(wand_fit_class)), 2)

  expect_equal(ncol(coef(wand_fit_reg)), 1)
  expect_equal(ncol(coef(wand_fit_reg_no_smooth)), 1)
  expect_equal(ncol(coef(wand_fit_reg_all_smooth)), 1)
  expect_equal(ncol(coef(wand_fit_class)), 3)
})

