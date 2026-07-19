test_that("tides_df() applies tides() to every row and preserves row count", {
  dat <- data.frame(
    mean             = c(4.2, 4.2, 1.2, 1.4),
    sd               = c(0.5, 0.5, 0.5, 0.6),
    n                = c(14, 14, 30, 35),
    min              = 1,
    max              = c(7, 7, 5, 7),
    n_items          = 1,
    digits           = 2,
    calculate_min_sd = TRUE,
    method           = c("exact", "approximate", "exact", "exact")
  )
  res <- tides_df(dat)
  expect_equal(nrow(res), nrow(dat))
  expect_true("tides_consistent" %in% names(res))
  expect_type(res$tides_consistent, "logical")
})

test_that("tides_df() keeps the original input columns", {
  dat <- data.frame(
    mean = 3.2, sd = 0.8, n = 30, min = 1, max = 5,
    n_items = 1, digits = 2, calculate_min_sd = TRUE, method = "exact"
  )
  res <- tides_df(dat)
  expect_true(all(c("mean", "sd", "n") %in% names(res)))
})
