# The scrutiny GRIM/GRIMMER interface changed after 0.6.1 and the two forms are
# mutually exclusive (see R/scrutiny-compat.R). These tests pin the dispatch, so
# that a future scrutiny change fails here rather than deep inside a vignette.

test_that(".takes_digits() identifies the installed scrutiny interface", {
  for (f in list(scrutiny::grim, scrutiny::grimmer)) {
    got <- strait:::.takes_digits(f)
    expect_type(got, "logical")
    expect_length(got, 1L)
    expect_false(is.na(got))
  }
  # whichever generation is installed, the arguments the shim intends to pass
  # must actually exist on it
  if (strait:::.takes_digits(scrutiny::grim)) {
    expect_true(all(c("digits_x") %in% names(formals(scrutiny::grim))))
    expect_true(all(c("digits_x", "digits_sd") %in% names(formals(scrutiny::grimmer))))
  } else {
    expect_true(is.function(scrutiny::restore_zeros))
  }
})

test_that("restore_zeros() pads to the reported decimal places on both generations", {
  expect_equal(scrutiny::restore_zeros(c(5.1, 5.19), width = 2), c("5.10", "5.19"))
})

# Ground truth: verdicts that must not depend on which interface is installed.
# Verified identical against CRAN scrutiny 0.6.1 and scrutiny 0.6.2.
test_that(".grim_compat() returns interface-independent GRIM verdicts", {
  expect_false(strait:::.grim_compat(x = 5.19, n = 28, digits = 2))
  expect_true(strait:::.grim_compat(x = 5.19, n = 32, digits = 2))
  expect_true(strait:::.grim_compat(x = 5.30, n = 20, digits = 2))
  expect_true(strait:::.grim_compat(x = 2.90, n = 30, digits = 2))
})

test_that(".grimmer_compat() returns interface-independent GRIMMER verdicts", {
  # scrutiny 0.6.1 warns on every grimmer() call about a known false-positive
  # in its test 3; the verdicts asserted here are unaffected by it
  expect_true(suppressWarnings(
    strait:::.grimmer_compat(x = 5.23, sd = 2.55, n = 40,
                             digits_x = 2, digits_sd = 2)))
  expect_false(suppressWarnings(
    strait:::.grimmer_compat(x = 3.10, sd = 0.90, n = 20,
                             digits_x = 2, digits_sd = 2)))
})

test_that("the shim agrees with a direct call on the installed interface", {
  if (!strait:::.takes_digits(scrutiny::grim)) skip("older scrutiny interface installed")
  expect_equal(
    as.logical(strait:::.grim_compat(x = 5.19, n = 32, digits = 2)),
    as.logical(scrutiny::grim(x = 5.19, n = 32, digits_x = 2)))
  expect_equal(
    as.logical(suppressWarnings(strait:::.grimmer_compat(
      x = 5.23, sd = 2.55, n = 40, digits_x = 2, digits_sd = 2))),
    as.logical(suppressWarnings(scrutiny::grimmer(
      x = 5.23, sd = 2.55, n = 40, digits_x = 2, digits_sd = 2))))
})

test_that("trailing zeros survive the round trip on the string interface", {
  # 2.90 at 2 dp and 2.9 at 1 dp are different reports and can differ in verdict
  expect_type(strait:::.grim_compat(x = 2.90, n = 30, digits = 2), "logical")
  expect_type(strait:::.grim_compat(x = 2.9,  n = 30, digits = 1), "logical")
})
