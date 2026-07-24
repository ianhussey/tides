test_that("sd_bounds_check() returns a verdict with POMP columns", {
  r <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 2.97, mean_digits = 2,
                       sd = 2.83, sd_digits = 2, Z = "integer")
  expect_true(all(c("consistent", "failed_tests", "min_sd", "max_sd",
                    "feasible", "grim", "grimmer", "sd_in_bounds",
                    "pomp_mean", "pomp_sd_parity", "pomp_sd_sharp",
                    "note") %in% names(r)))
  expect_true(r$consistent)
  expect_equal(r$failed_tests, "")
})

test_that("an SD below the floor fails the bounds test", {
  r <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 2.97, mean_digits = 2,
                       sd = 0.10, sd_digits = 2, Z = "quasiinteger")
  expect_false(r$consistent)
  expect_true(grepl("bounds", r$failed_tests))
  expect_lt(r$pomp_sd_sharp, 0)          # below the band -> negative
})

test_that("a GRIM-impossible mean fails feasibility", {
  r <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 3.51, mean_digits = 2,
                       sd = 1.00, sd_digits = 2, Z = "integer")
  expect_false(r$consistent)
  expect_true(grepl("feasibility", r$failed_tests))
})

test_that("pomp_sd_parity is linear and mean-independent", {
  p1 <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 4, sd = 1.0, sd_digits = 6,
                        rounding = NULL, Z = "continuous")
  p2 <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 4, sd = 2.0, sd_digits = 6,
                        rounding = NULL, Z = "continuous")
  p3 <- sd_bounds_check(l = 1, u = 7, n = 30, mean = 6, sd = 1.0, sd_digits = 6,
                        rounding = NULL, Z = "continuous")
  expect_equal(p2$pomp_sd_parity, 2 * p1$pomp_sd_parity)
  expect_equal(p3$pomp_sd_parity, p1$pomp_sd_parity)
})

test_that("pomp_sd_sharp is 1 at the ceiling and 0 at the floor", {
  mx <- sd_bounds(l = 1, u = 5, n = 9, mean = 3)$max_sd
  mn <- sd_bounds(l = 1, u = 5, n = 9, mean = 2.44, Z = "quasiinteger")$min_sd
  sh1 <- sd_bounds_check(l = 1, u = 5, n = 9, mean = 3, sd = mx, sd_digits = 9,
                         rounding = NULL, Z = "continuous")$pomp_sd_sharp
  sh0 <- sd_bounds_check(l = 1, u = 5, n = 9, mean = 2.44, sd = mn, sd_digits = 9,
                         rounding = NULL, Z = "quasiinteger")$pomp_sd_sharp
  expect_equal(sh1, 1, tolerance = 1e-6)
  expect_equal(sh0, 0, tolerance = 1e-6)
})

test_that("sd_bounds_check_multiple() equals a row-wise loop, incl. duplicates", {
  df <- expand.grid(mean = c(2.90, 2.97, 3.50), sd = c(0.10, 1.50, 2.83))
  df <- rbind(df, df[1:3, ])
  mult <- sd_bounds_check_multiple(df, l = 1, u = 7, n = 30, mean_digits = 2,
                                   sd_digits = 2, Z = "integer")
  roww <- do.call(rbind, lapply(seq_len(nrow(df)), function(i)
    sd_bounds_check(l = 1, u = 7, n = 30, mean = df$mean[i], mean_digits = 2,
                    sd = df$sd[i], sd_digits = 2, Z = "integer")))
  expect_equal(mult$consistent, roww$consistent)
  expect_equal(mult$min_sd, roww$min_sd)
  expect_equal(mult$pomp_sd_sharp, roww$pomp_sd_sharp)
})

test_that("sd_bounds_check_multiple() honours include_inputs and column/constant collisions", {
  df <- data.frame(mean = c(3, 4), sd = c(1.5, 1.6))
  keep <- sd_bounds_check_multiple(df, l = 1, u = 7, n = 30, mean_digits = 1,
                                   sd_digits = 1, Z = "integer")
  drop <- sd_bounds_check_multiple(df, l = 1, u = 7, n = 30, mean_digits = 1,
                                   sd_digits = 1, Z = "integer",
                                   include_inputs = FALSE)
  expect_true(all(c("mean", "sd") %in% names(keep)))
  expect_false("mean" %in% setdiff(names(drop), names(keep)))
  expect_equal(nrow(drop), nrow(df))
  df$n <- 30
  expect_error(sd_bounds_check_multiple(df, n = 30, l = 1, u = 7,
                                        mean_digits = 1, sd_digits = 1, sd = 1))
})
