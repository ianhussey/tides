l <- 1; u <- 5; n <- 7
m <- seq(l, u, by = 0.002)
bes <- sqrt(n / (n - 1))

test_that("band rules reproduce the article's nested-constraint panels", {
  # Popoviciu, range only
  d <- sd_region_data(l, u, n, rule = "range", by = 0.002)
  expect_equal(d$lo, rep(0, length(m)))
  expect_equal(d$hi, rep((u - l) / sqrt(2), length(m)))

  # Muilwijk / Bhatia-Davis arch, no floor
  d <- sd_region_data(l, u, n, rule = "mean", by = 0.002)
  expect_equal(d$hi, bes * sqrt(pmax(0, (u - m) * (m - l))))
  expect_equal(d$lo, rep(0, length(m)))

  # that arch plus the naive Bernoulli floor
  d <- sd_region_data(l, u, n, rule = "mean_naive_floor", by = 0.002)
  expect_equal(d$lo, bes * sqrt((m - floor(m)) * (1 - (m - floor(m)))))
  expect_equal(d$hi, bes * sqrt(pmax(0, (u - m) * (m - l))))

  # this article: both bounds sharp, GRIM-free
  d <- sd_region_data(l, u, n, rule = "quasi", by = 0.002)
  expect_equal(d$lo, sd_min_quasi_integer(m, n))
  expect_equal(d$hi, sd_max_structure_s(m, n, l, u))
})

test_that("the alpha rule matches the article's alpha panel", {
  k <- 2; a <- 0.70; cc <- (k - 1) / k; D <- 1 - cc * a
  qmin_k <- sd_min_quasi_integer(k * (m - l), n) / k
  qmax_k <- sd_max_structure_s(k * (m - l), n, 0, k * (u - l)) / k
  ceil_a <- sqrt((n / (n - 1)) * v_max_alpha(k * m, k, n, l, u) / D) / k
  exp_lo <- qmin_k / sqrt(D)
  exp_hi <- pmin(ceil_a, qmax_k)
  bad <- exp_lo > exp_hi + 1e-12
  exp_lo[bad] <- NA_real_; exp_hi[bad] <- NA_real_

  d <- sd_region_data(l, u, n, rule = "alpha", n_items = k, alpha = a, by = 0.002)
  expect_equal(d$lo, exp_lo)
  expect_equal(d$hi, exp_hi)
})

test_that("alpha only tightens, and is inert / rejected at one item", {
  free  <- sd_region_data(l, u, n, rule = "quasi", n_items = 2, by = 0.01)
  withA <- sd_region_data(l, u, n, rule = "alpha", n_items = 2, alpha = 0.7, by = 0.01)
  ok <- !is.na(withA$hi)
  expect_true(all(withA$hi[ok] <= free$hi[ok] + 1e-9))
  expect_true(all(withA$lo[ok] >= free$lo[ok] - 1e-9))
  expect_error(sd_region_data(l, u, n, rule = "alpha", n_items = 1, alpha = 0.7))
  expect_error(sd_region_data(l, u, n, rule = "alpha"))          # alpha required
  expect_error(sd_region_data(l, u, n, rule = "integer_alpha"))  # alpha required
})

test_that("lattice rules return attainable tuples, and alpha is a strict subset", {
  a  <- sd_region_data(l, u, n, rule = "integer", n_items = 2, digits = 2)
  b  <- sd_region_data(l, u, n, rule = "integer_alpha", n_items = 2,
                       alpha = 0.70, digits = 2)
  expect_identical(attr(a, "type"), "points")
  expect_named(a, c("mean", "sd"))
  expect_gt(nrow(a), 0)
  expect_lt(nrow(b), nrow(a))                                   # alpha removes tuples
  expect_true(all(paste(b$mean, b$sd) %in% paste(a$mean, a$sd))) # strict subset

  # every retained tuple really is inside the alpha-conditional bounds
  keep <- b[seq(1, nrow(b), length.out = min(40, nrow(b))), ]
  ok <- vapply(seq_len(nrow(keep)), function(i) {
    r <- sd_bounds(l = l, u = u, n = n, mean = keep$mean[i], mean_digits = 2,
                   rounding = "up_or_down", Z = "integer", scoring = "meanscored",
                   n_items = 2, alpha = 0.70)
    isTRUE(r$feasible) &&
      (keep$sd[i] + 0.005) >= r$min_sd - 1e-9 &&
      (keep$sd[i] - 0.005) <= r$max_sd + 1e-9
  }, logical(1))
  expect_true(all(ok))
})

test_that("every rule returns a ggplot", {
  for (r in c("range", "range_n", "mean", "mean_naive_floor", "mestdagh",
              "pesant_regin", "quasi")) {
    expect_s3_class(plot_sd_region(l, u, n, rule = r), "ggplot")
  }
  expect_s3_class(plot_sd_region(l, u, n, rule = "alpha", n_items = 2, alpha = 0.7),
                  "ggplot")
  expect_s3_class(plot_sd_region(l, u, n, rule = "integer", digits = 1), "ggplot")
  expect_s3_class(plot_sd_region(l, u, n, rule = "integer_alpha", n_items = 2,
                                 alpha = 0.7, digits = 1), "ggplot")
})

test_that("sd_max_muilwijk is the smooth arch and never below Structure S", {
  expect_equal(sd_max_muilwijk(3, n, l, u), bes * sqrt((u - 3) * (3 - l)))
  expect_true(all(sd_max_muilwijk(m, n, l, u) >= sd_max_structure_s(m, n, l, u) - 1e-9))
})
