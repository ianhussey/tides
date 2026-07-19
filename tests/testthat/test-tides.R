test_that("tides() returns the documented output columns", {
  res <- tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5)
  expect_s3_class(res, "data.frame")
  expect_equal(nrow(res), 1L)
  expect_true(all(c(
    "relative_location", "relative_dispersion", "min_sd", "max_sd",
    "sd_range_calculable", "mean_inside_range", "sd_inside_range",
    "inside_ranges", "tides_consistent"
  ) %in% names(res)))
})

test_that("a feasible mean/SD pair is flagged consistent", {
  res <- tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5)
  expect_true(res$tides_consistent)
})

test_that("an SD larger than the maximum possible is flagged inconsistent", {
  res <- tides(mean = 3.0, sd = 5, n = 30, min = 1, max = 5)
  expect_false(res$tides_consistent)
  expect_false(res$sd_inside_range)
})

test_that("verbose = TRUE prepends the input arguments", {
  quiet   <- tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5, verbose = FALSE)
  verbose <- tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5, verbose = TRUE)
  expect_true(ncol(verbose) > ncol(quiet))
  expect_true(all(c("mean", "sd", "n", "min", "max", "method") %in% names(verbose)))
  expect_false("mean" %in% names(quiet))
})

test_that("the approximate method returns bounds where the exact method cannot", {
  res <- tides(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7, method = "approximate")
  expect_true(res$sd_range_calculable)
})

test_that("method is validated via match.arg()", {
  expect_error(tides(mean = 3.2, sd = 0.8, n = 30, min = 1, max = 5, method = "bogus"))
})
