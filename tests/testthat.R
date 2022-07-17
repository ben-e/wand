library(testthat)
library(wand)

if (torch::torch_is_installed()) {
  test_check("wand")
}
