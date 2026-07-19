test_that("sd_bounds() returns a one-row data frame with min_sd and max_sd", {
  b <- sd_bounds(mean = 3.2, n = 30, min = 1, max = 5)
  expect_s3_class(b, "data.frame")
  expect_equal(nrow(b), 1L)
  expect_true(all(c("min_sd", "max_sd") %in% names(b)))
})

test_that("sd_bounds() computes the expected bounds for a known case", {
  b <- sd_bounds(mean = 3.2, n = 30, min = 1, max = 5)
  expect_equal(b$min_sd, 0.4)
  expect_equal(b$max_sd, 2)
})

test_that("the lower bound is never greater than the upper bound", {
  b <- sd_bounds(mean = 4.1, n = 25, min = 1, max = 7)
  expect_lte(b$min_sd, b$max_sd)
})

test_that("calculate_min_sd = FALSE forces min_sd to 0", {
  b <- sd_bounds(mean = 3.2, n = 30, min = 1, max = 5, calculate_min_sd = FALSE)
  expect_equal(b$min_sd, 0)
  expect_false(is.na(b$max_sd))
})

test_that("return_distributions adds the achieving vectors", {
  b <- sd_bounds(
    mean = 3.2,
    n = 30,
    min = 1,
    max = 5,
    return_distributions = TRUE
  )
  expect_true(all(c("min_dist", "max_dist") %in% names(b)))
  expect_length(b$max_dist[[1]], 30)
})
