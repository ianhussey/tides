<img src="man/figures/logo.png" width="30%" alt="tides logo" />

# tides: Truncation-Induced Dependency among Summary Statistics

<!-- badges: start -->
[![R-CMD-check](https://github.com/ianhussey/TIDES/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ianhussey/TIDES/actions/workflows/R-CMD-check.yaml)

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21439905.svg)](https://doi.org/10.5281/zenodo.21439905)
<!-- badges: end -->

`tides` is a forensic meta-science R package for conducting trustworthiness assessments on reported rounded summary statistics using bounds checks. 

When a measure has a known minimum and maximum score, the reported mean constrains the standard deviation that is arithmetically possible: the two summary statistics are not independent. Given a reported mean, standard deviation and sample size, `tides()` computes the smallest and largest standard deviations that could have produced that mean under the scale's bounds, and flags a report as inconsistent when the reported SD falls outside that feasible range (following the terminology used by related forensic-metascience methods such as GRIM and GRIMMER).

## The problem

A measure is truncated when its scores are confined to a known interval `[min, max]` — for example a 1–7 Likert item, a 0–63 depression inventory, or a percentage bounded at 0 and 100. For a sample of size `n` with reported mean `M`, only certain standard deviations are attainable:

- the **largest** SD comes from pushing observations toward the two extremes (`min` and `max`) while still averaging to `M`;
- the **smallest** SD comes from concentrating observations as tightly as possible around `M`.

Any reported SD outside `[min_sd, max_sd]` cannot have arisen from real data on that scale.

| reported SD relative to bounds | verdict |
|---|---|
| inside `[min_sd, max_sd]` | **consistent** |
| outside `[min_sd, max_sd]` | **inconsistent** |
| bounds not calculable for that mean | **not testable at this precision** (see the two methods below) |

## The method

For a reported mean `M`, sample size `n` and scale range `[min, max]`, `sd_bounds()` constructs the discrete distributions that minimise and maximise the SD subject to averaging (after rounding) to `M`, and returns the resulting `min_sd` and `max_sd`. `tides()` then checks the reported SD against those bounds and also reports Percent-Of-Maximum-Possible (POMP) transforms of the mean (`relative_location`) and SD (`relative_dispersion`) so results from different scales can be compared on one axis.

Two ways of handling a mean whose exact SD bounds cannot be computed are available via the `method` argument:

- **`"exact"`** (default): bounds are computed for the reported mean only. Because a mean–SD pair with no feasible integer-sum distribution simply has no bounds, the exact test *implicitly* also enforces GRIM and GRIMMER consistency: passing means the values are jointly GRIM-, GRIMMER- and truncation-consistent.
- **`"approximate"`**: when the exact mean has no calculable bounds, the bounds of the nearest mean that does are used instead. This isolates the truncation (TIDES) test on its own, ignoring GRIM/GRIMMER granularity constraints — a more liberal check against the extreme feasible SDs of nearby means.

`umbrella()` enumerates the full grid of reported means and SDs that are *simultaneously* GRIM-, GRIMMER- and TIDES-consistent for a given design, which `plot_umbrella()` renders as the characteristic "umbrella" of feasible values.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ianhussey/TIDES")
```

## Usage

See also the vignette in the R package (`vignette("tides")`).

```r
library(tides)

# A single reported mean and SD on a 1-5 scale
tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5)

# A value can sit inside the TIDES envelope yet still be (implicitly) GRIM/GRIMMER
# inconsistent under the default "exact" method:
tides(mean = 3.21, sd = 0.80, n = 30, min = 1, max = 5)

# The more liberal, TIDES-only test against nearby means:
tides(mean = 3.21, sd = 0.80, n = 30, min = 1, max = 5, method = "approximate")
```

### Checking a table of reported statistics

`tides_df()` applies the check row-by-row over a data frame, taking its arguments from columns of the same name:

```r
dat <- data.frame(
  mean             = c(4.2, 4.2, 1.2, 1.4),
  sd               = c(0.5, 0.5, 0.5, 0.6),
  n                = c( 14,  14,  30,  35),
  min              = 1,
  max              = c(7, 7, 5, 7),
  n_items          = 1,
  digits           = 2,
  calculate_min_sd = TRUE,
  method           = c("exact", "approximate", "exact", "exact")
)

tides_df(dat)
```

### Visualising the feasible region

```r
# The TIDES envelope for a single design, with reported points overlaid
tides(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7) |>
  plot_tides()

# Pool reports from different scales on a standardised relative-POMP plot
plot_tides_relative(tides_df(dat))

# The umbrella of all jointly GRIM + GRIMMER + TIDES consistent values
umbrella(n = 14, min = 1, max = 7, digits = 2) |>
  plot_umbrella()
```

## API

| function | purpose |
|---|---|
| `tides(mean, sd, n, min, max, ...)` | main entry point; test a single reported mean/SD/n against the feasible SD range for a truncated scale |
| `sd_bounds(mean, n, min, max, ...)` | the smallest and largest SD attainable for a reported mean on a bounded scale |
| `tides_df(.data, ...)` | apply `tides()` to each row of a data frame |
| `approximate_sd_bounds(dat)` | relax discontinuities in the SD-bound curves by filling from nearby means (used by the `"approximate"` method and the plots) |
| `umbrella(n, min, max, ...)` | enumerate all jointly GRIM-, GRIMMER- and TIDES-consistent (mean, SD) pairs for a design |
| `plot_tides(res, ...)` | plot the TIDES envelope with reported points overlaid |
| `plot_tides_relative(res, ...)` | plot results on a standardised relative (POMP) scale so different designs can be pooled |
| `plot_umbrella(dat, ...)` | scatterplot of the feasible (mean, SD) umbrella |

## Limitations

- **`n_items` above 1 is not yet supported.** The multi-item (within-participant averaging) path is still under development; `n_items` is currently forced to `1` internally by `tides()`, `tides_df()` and `umbrella()`. Treat the tools as applying to single-item / participant-level means for now.

## Suggested citation

Hussey, I. (2024). tides: Trustworthiness assessments for summary statistics using bounds checks. https://github.com/ianhussey/tides doi: [10.5281/zenodo.21439905](https://doi.org/10.5281/zenodo.21439905)

## References

- N. J. L. Brown and J. A. J. Heathers (2017), "The GRIM test: A simple technique detects numerous anomalies in the reporting of results in psychology," *Social Psychological and Personality Science*, 8(4):363–369. [doi:10.1177/1948550616673876](https://doi.org/10.1177/1948550616673876)

## License

Code is MIT licensed © Ian Hussey.
