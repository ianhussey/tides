test_that("umbrella() returns feasible (mean, sd) pairs within the scale", {
  skip_on_cran()
  u <- umbrella(n = 14, min = 1, max = 7, digits = 2)
  expect_s3_class(u, "data.frame")
  expect_true(all(c("mean", "sd") %in% names(u)))
  expect_gt(nrow(u), 0)
  expect_true(all(u$mean >= 1 & u$mean <= 7))
  expect_true(all(u$sd >= 0))
})

test_that("plot_umbrella() returns a ggplot object", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  u <- umbrella(n = 14, min = 1, max = 7, digits = 2)
  expect_s3_class(plot_umbrella(u), "ggplot")
})
