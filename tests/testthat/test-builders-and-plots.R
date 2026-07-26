test_that("sd_bounds_curve() is hole-free under quasi-integer", {
  cv <- sd_bounds_curve(l = 1, u = 7, n = 30, Z = "quasiinteger")
  expect_true(all(c("mean", "min_sd", "max_sd", "feasible", "pomp_mean",
                    "parity_max", "ceil_parity", "floor_parity") %in% names(cv)))
  expect_false(any(is.na(cv$min_sd[cv$feasible])))
  expect_false(any(is.na(cv$max_sd[cv$feasible])))
})

test_that("umbrella_data() passing set matches an independent GRIMMER + bounds screen", {
  # kept small deliberately: CRAN scrutiny (0.6.1) evaluates GRIMMER about 25x
  # slower than 0.6.2, so a wide grid here costs minutes on the CRAN path
  um <- umbrella_data(n = 12, l = 1, u = 3, digits = 1)
  expect_true(all(c("mean", "sd", "min_sd", "max_sd", "in_bounds",
                    "grimmer", "consistent") %in% names(um)))
  # all three verdict categories must be present, or this proves little
  expect_true(any(um$consistent))
  expect_true(any(um$in_bounds & !is.na(um$grimmer) & !um$grimmer))
  expect_true(any(!um$in_bounds))
  indep <- with(um, {
    gm <- as.logical(strait:::.grimmer_compat(x = mean, sd = sd, n = 12,
                                              digits_x = 1, digits_sd = 1,
                                              items = 1,
                                              rounding = "up_or_down"))
    ib <- (sd + 0.05) >= min_sd - 1e-9 & (sd - 0.05) <= max_sd + 1e-9
    gm & ib
  })
  expect_equal(um$consistent, indep)
})

test_that("the plot functions return ggplot objects", {
  cv  <- sd_bounds_curve(l = 1, u = 7, n = 30, Z = "quasiinteger")
  pts <- brimmer_multiple(
    data.frame(mean = c(3, 4, 5), sd = c(2.5, 0.2, 1.8)),
    l = 1, u = 7, n = 30, mean_digits = 1, sd_digits = 1, Z = "quasiinteger")
  expect_s3_class(plot_sd_bounds(cv, points = pts), "ggplot")
  expect_s3_class(plot_sd_bounds_pomp(cv, points = pts, reference = "parity"), "ggplot")
  expect_s3_class(plot_sd_bounds_pomp(cv, points = pts, reference = "sharp"), "ggplot")
  um <- umbrella_data(n = 12, l = 1, u = 7, digits = 2)
  expect_s3_class(plot_umbrella(um, curve = sd_bounds_curve(l = 1, u = 7, n = 12,
                                                            Z = "quasiinteger")), "ggplot")
})
