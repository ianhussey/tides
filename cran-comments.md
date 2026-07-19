## Submission

This is a new submission.

## Test environments

- local macOS (aarch64), R 4.5.2 (release)
- GitHub Actions: macOS, Windows, Ubuntu (R-devel, release, oldrel-1)
- win-builder (R-devel), via `devtools::check_win_devel()`

## R CMD check results

0 errors | 0 warnings | 1 note.

The one NOTE is the standard "New submission" flag.

## Notes for the reviewer

* The package uses domain terms and author names that a spell-checker does not
  recognise but that are spelled correctly and intentional: GRIM, GRIMMER,
  POMP, truncation-consistent, metascience, and the cited authors Brown and
  Heathers. These are recorded in `inst/WORDLIST`, so
  `spelling::spell_check_package()` passes with no spelling NOTE. The reference
  in the Description is given as `Authors (year) <doi:...>`.
* The package has no compiled code and writes no files outside `tempdir()`.
* Examples that build ggplot objects or enumerate feasibility grids are wrapped
  in `\donttest{}` to keep the per-example runtime within CRAN limits; they run
  in full locally and in CI.
* `umbrella()` and the vignette call the `scrutiny` package's GRIM/GRIMMER
  functions, whose argument interface has changed across scrutiny releases. The
  package detects the installed interface at run time (see
  `R/scrutiny-compat.R`), so it works with both current CRAN scrutiny and newer
  versions.
