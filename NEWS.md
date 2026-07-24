# tides 0.4.0

A ground-up rewrite of the engine around the closed-form standard-deviation
bounds derived in the TIDES article. This is a breaking change: the API is new
throughout.

## Breaking changes

* Removed `tides()`, `tides_df()`, `umbrella()`, `approximate_sd_bounds()`,
  `plot_tides()` and `plot_tides_relative()`, and the internal
  `grimmer_consistent()` scrutiny shim.
* `sd_bounds()` has a new signature and semantics. It now takes logical scale
  limits `l`, `u` (formerly `min`, `max`), optional attained extremes `a`, `b`,
  a granularity argument `Z` (`"continuous"`, `"integer"`, `"quasiinteger"`),
  `scoring` (`"singleitem"`, `"sumscored"`, `"meanscored"`) with `n_items`, and
  an optional Cronbach's `alpha`. It returns bounds in closed form (no
  constructed distributions) together with the binding rule for each bound.
* `plot_umbrella()` now takes the output of `umbrella_data()`.

## New features

* Bounds are now sharp under many more constraint sets: attained observed
  extremes (`a`, `b`), the quasi-integer floor (defined at every mean, so bound
  curves are hole-free), the sharp Structure-S mean-conditional ceiling, and a
  reported Cronbach's alpha for sum- or mean-scored composites.
* `n_items` greater than 1 is now fully supported via the granularity grid
  (previously forced to 1).
* Rounded and truncated reported values are handled explicitly
  (`unround_interval()`, and a `rounding` argument with the usual scrutiny-style
  options); GRIM and GRIMMER verdicts are deferred to `scrutiny`.
* `sd_bounds_check()` gives a single consistent/inconsistent verdict, the
  failing tests, and percent-of-maximum-possible transforms (`pomp_mean`,
  `pomp_sd_parity`, `pomp_sd_sharp`). `sd_bounds_check_multiple()` applies it
  across a data frame, de-duplicating repeated constraint tuples.
* `sd_bounds_curve()` and `umbrella_data()` build the plotting data;
  `plot_sd_bounds()`, `plot_sd_bounds_pomp()` (native and POMP scales) and
  `plot_umbrella()` visualise it.
* The alpha-conditional floor is sharpened for strictly integer composites via
  the composite's Gini mean difference (`sd_min_alpha_gini()`, TIDES article
  Theorem H5): a reported positive alpha now yields a strictly positive minimum
  SD even at whole-number sum-score means, where the earlier amplified floor
  vanished. The exact envelope is used when the composite range is small enough
  to enumerate; otherwise the proven amplified floor is retained (flagged in
  `note`).
* The single-purpose bound primitives (e.g. `sd_max_structure_s()`,
  `sd_min_quasi_integer()`) are exported and documented.
* `sd_bounds_sample()` constructs a sample that attains the maximum or minimum
  SD bound (the constructive companion to the closed-form `sd_bounds()`),
  replacing the old constructive `return_distributions` output.
* `v_max_alpha()` (the Appendix H allocation maximum of the item-variance sum)
  is now exported for reuse.
* `plot_sd_region()` and `sd_region_data()` draw the feasible SD region for any
  one constraint set in the nested framework, reproducing the panels of the
  article's nested-constraints figure from a single entry point: `"range"`,
  `"range_n"`, `"mean"`, `"mean_naive_floor"`, `"pesant_regin"`, `"mestdagh"`,
  `"quasi"`, `"alpha"`, plus two strictly integer views, `"integer"` and
  `"integer_alpha"`, which plot the lattice of GRIM- and GRIMMER-attainable
  reported tuples (no band exists at means integer data cannot produce). Each
  panel can carry the sharp alpha-free quasi-integer band as a dashed reference.
* `sd_max_muilwijk()` (the smooth Muilwijk / Bhatia-Davis arch) is exported.
* `umbrella_data()` gains an `alpha` argument, so the grid can be restricted to
  the jointly GRIM-, GRIMMER- and alpha-consistent tuples.

## Dependencies

* `Imports` trimmed to `ggplot2` and `scrutiny` (dropping `dplyr`, `tidyr`,
  `purrr`, `tibble`, `janitor`, `forcats`, `scales`, `rlang`).

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
