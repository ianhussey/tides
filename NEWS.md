# tides 0.3.2

This release prepares the package for CRAN and fixes several bugs that affected
the installed package.

## Bug fixes

* Core functions (`tides()`, `tides_df()`, `umbrella()`, `approximate_sd_bounds()`)
  no longer error when the package is installed: functions that were used but
  never imported (e.g. `dplyr::case_when()`, `dplyr::mutate_if()`,
  `dplyr::arrange()`) are now imported or fully qualified.
* `umbrella()` and the vignette now work across `scrutiny` versions. The
  GRIM/GRIMMER interface changed across `scrutiny` releases (older versions take
  zero-padded strings; newer ones take numbers plus their decimal places); the
  installed interface is now detected at run time (`grimmer_consistent()`).
* `plot_umbrella()` no longer warns about a missing `digits` column; it gains a
  `digits` argument (defaulting to a `digits` column when present, otherwise 2).

## New features and changes

* `tides_df()` now falls back to the `tides()` defaults when the optional
  columns `n_items`, `digits`, `calculate_min_sd` or `method` are absent, so an
  input data frame need not carry them.

## Documentation

* Rewrote the README with badges and problem / method / installation / usage /
  API / limitations / citation sections, and expanded the `DESCRIPTION`.
* Added a testthat test suite, a GitHub Actions `R-CMD-check` workflow,
  `cran-comments.md`, `inst/WORDLIST`, and `LICENSE.md`.
* Moved the package-level documentation to `R/tides-package.R` (from `zzz.R`),
  made all Rd content ASCII, and made examples runnable (`\donttest` where heavy).

## Known limitations

* `n_items` greater than 1 is not yet supported and is currently forced to 1
  internally in `tides()`, `tides_df()` and `umbrella()`.
