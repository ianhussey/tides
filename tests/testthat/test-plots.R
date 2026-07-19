test_that("plot_tides() returns a ggplot for a single tides() result", {
  skip_if_not_installed("ggplot2")
  res <- tides(mean = 3.10, sd = 0.80, n = 100, min = 1, max = 7)
  expect_s3_class(plot_tides(res), "ggplot")
})

test_that("plot_tides_relative() requires an approximate-method input", {
  skip_if_not_installed("ggplot2")
  dat <- data.frame(
    mean = c(3.1, 5.5),
    sd = c(0.8, 2.9),
    n = 30,
    min = 1,
    max = 7,
    n_items = 1,
    digits = 2,
    calculate_min_sd = TRUE,
    method = "approximate"
  )
  res <- tides_df(dat)
  expect_s3_class(plot_tides_relative(res), "ggplot")

  exact <- tides(mean = 3.2, sd = 0.8, n = 30, min = 1, max = 5)
  expect_error(plot_tides_relative(exact))
})
