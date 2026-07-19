# Version-robust wrapper around scrutiny::grimmer().
#
# The scrutiny GRIMMER interface changed across releases: CRAN scrutiny
# (<= 0.6.1) takes the reported mean and SD as zero-padded strings, i.e.
# grimmer(x, sd, n, items, rounding); later versions take them as numbers
# together with their reported decimal places, i.e. grimmer(x, sd, n,
# digits_x, digits_sd, items, rounding). We detect which interface the
# installed version exposes and dispatch accordingly, so umbrella() works
# regardless of the scrutiny version a user has installed.
#
# Returns a logical vector, one element per (mean, sd) pair.
grimmer_consistent <- function(mean, sd, n, digits, n_items = 1, rounding = "up") {
  has_digits <- "digits_x" %in% names(formals(scrutiny::grimmer))

  if (has_digits) {
    purrr::pmap_lgl(
      list(x = mean, sd = sd, n = n),
      function(x, sd, n) {
        scrutiny::grimmer(
          x = x,
          sd = sd,
          n = n,
          digits_x = digits,
          digits_sd = digits,
          items = n_items,
          rounding = rounding
        )
      }
    )
  } else {
    # Older interface: pass zero-restored character values.
    purrr::pmap_lgl(
      list(
        x = scrutiny::restore_zeros(mean, width = digits),
        sd = scrutiny::restore_zeros(sd, width = digits),
        n = n
      ),
      function(x, sd, n) {
        scrutiny::grimmer(
          x = x,
          sd = sd,
          n = n,
          items = n_items,
          rounding = rounding
        )
      }
    )
  }
}
