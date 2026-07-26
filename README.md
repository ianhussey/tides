<img src="man/figures/logo.png" width="30%" alt="strait logo" />

# strait: Bounds checks for reported summary statistics 

<!-- badges: start -->
[![R-CMD-check](https://github.com/ianhussey/strait/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ianhussey/strait/actions/workflows/R-CMD-check.yaml)

[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.21439905.svg)](https://doi.org/10.5281/zenodo.21439905)
<!-- badges: end -->

`strait` is a forensic meta-science R package for conducting trustworthiness assessments on reported rounded summary statistics using bounds checks - to assess whether reported summary stats are on the *strait and narrow*. 

When a measure has a known minimum and maximum score, the reported mean constrains the standard deviation that is arithmetically possible: the two summary statistics are not independent. Given a reported mean, standard deviation and sample size, `brimmer()` computes the smallest and largest standard deviations that could have produced that mean under the scale's bounds, and flags a report as inconsistent when the reported SD falls outside that feasible range (following the terminology used by related forensic-metascience methods such as GRIM and GRIMMER).

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

For a reported mean `M`, sample size `n` and scale limits `l`, `u`, `sd_bounds()` returns, **in closed form**, the smallest and largest sample SD (`min_sd`, `max_sd`) consistent with the constraints supplied, and names the binding rule for each. The constraints are nested — each one can only tighten the bounds:

- scale limits `l`, `u` give the largest possible SD (observations pushed to the two extremes);
- adding `n`, then the mean, sharpens the ceiling further;
- **attained extremes** `a`, `b` (an observation is known to equal each) create a nonzero floor;
- **granularity** `Z` (`"integer"` or the GRIM-free `"quasiinteger"`) adds the floor below which no discrete data can sit;
- a reported **Cronbach's alpha** tightens the bounds of a multi-item composite.

The two tests mirror the GRIM / GRIMMER pair, and nest the same way:

- **`brim()`** — the mean-side test. Can the reported mean be attained at all, given the scale limits, `n`, and any reported attained extremes? Bounds only, so it applies to continuous data too. Under `Z = "integer"` it also runs GRIM (deferred to `scrutiny`), but never GRIMMER.
- **`brimmer()`** — the SD-side test, nested on top of `brim()`: it applies the mean-side check and then asks whether the reported SD lies inside `[min_sd, max_sd]`.

Each failure is named separately in `failed_tests`, so an out-of-range mean (`in_scale_range`) is never confused with a granularity-impossible one (`grim`):

```r
brim(l = 1, u = 7, n = 30, mean = 3.51, mean_digits = 2, Z = "integer")
#>   consistent failed_tests in_scale_range  grim band_lo band_hi
#> 1      FALSE         grim           TRUE FALSE       1       7
```

`brimmer()` also adds Percent-Of-Maximum-Possible (POMP) transforms of the mean and SD so results from different scales can be compared on one axis. `brimmer_multiple()` applies it across a data frame.

`umbrella_data()` builds the full grid of reported means and SDs for a design, tagging each as consistent, GRIMMER-inconsistent or out of bounds, which `plot_umbrella()` renders as the characteristic "umbrella" of feasible values.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ianhussey/strait")
```

## Usage

See also the vignette in the R package (`vignette("strait")`).

```r
library(strait)

# The feasible SD range for a reported mean on a 1-5 integer scale
sd_bounds(l = 1, u = 5, n = 30, mean = 3.2, Z = "integer")

# Check a reported mean alone: is it attainable within the scale at all?
brim(l = 1, u = 5, n = 30, mean = 3.20, mean_digits = 2, Z = "integer")

# Check a reported mean and SD (as printed in a paper: value + decimal places)
brimmer(l = 1, u = 5, n = 30, mean = 3.20, mean_digits = 2,
        sd = 0.80, sd_digits = 2, Z = "integer")
```

### Checking a table of reported statistics

`brimmer_multiple()` applies the check row-by-row over a data frame, taking per-row values from columns of the same name and broadcasting anything passed as a constant:

```r
dat <- data.frame(
  mean = c(4.2, 4.2, 1.2, 1.4),
  sd   = c(0.5, 0.5, 0.5, 0.6),
  n    = c( 14,  14,  30,  35),
  u    = c(  7,   7,   5,   7)
)

brimmer_multiple(dat, l = 1, mean_digits = 1, sd_digits = 1, Z = "integer")
```

### Visualising the feasible region

```r
# The SD-bounds envelope for a single design, with reported points overlaid
curve  <- sd_bounds_curve(l = 1, u = 7, n = 15, Z = "quasiinteger")
points <- brimmer_multiple(
  data.frame(mean = 5.07, sd = 2.92),
  l = 1, u = 7, n = 15, mean_digits = 2, sd_digits = 2, Z = "quasiinteger")
plot_sd_bounds(curve, points = points)

# The same on a standardised POMP scale so different designs can be pooled
plot_sd_bounds_pomp(curve, points = points, reference = "sharp")

# The umbrella of all jointly GRIM + GRIMMER + bounds consistent values
umbrella_data(n = 14, l = 1, u = 7, digits = 2) |>
  plot_umbrella()
```

## API

| function | purpose |
|---|---|
| `sd_bounds(l, u, a, b, n, mean, Z, scoring, n_items, alpha, ...)` | the smallest and largest sample SD consistent with the constraints supplied, in closed form |
| `brim(l, u, a, b, n, mean, mean_digits, Z, ...)` | the mean-side test: is the reported mean attainable within the scale limits (and, under `Z = "integer"`, GRIM)? |
| `brimmer(...)` | the SD-side test, nested on `brim()`: turn the bounds into a consistent/inconsistent verdict with POMP transforms; defers GRIM/GRIMMER to `scrutiny` |
| `brimmer_multiple(data, ...)` | apply `brimmer()` to each row of a data frame |
| `sd_bounds_curve(l, u, n, ...)` | trace the floor and ceiling of the SD across the mean (hole-free under `"quasiinteger"`) |
| `umbrella_data(n, l, u, ...)` | build the grid of reported (mean, SD) pairs with their consistency verdicts |
| `plot_sd_bounds(curve, ...)` | plot the SD-bounds envelope on the native scale, with reported points |
| `plot_sd_bounds_pomp(curve, ...)` | plot on a POMP scale (`reference = "parity"` or `"sharp"`) so designs can be pooled |
| `plot_umbrella(umbrella, ...)` | plot the feasible (mean, SD) umbrella |

The single-purpose bound primitives (e.g. `sd_max_structure_s()`, `sd_min_quasi_integer()`, `unround_interval()`) are also exported.

## Limitations

- **A small residual blind spot at the umbrella's edge.** A handful of reported (mean, SD) tuples pass GRIM, GRIMMER *and* the SD bounds yet still have no integer-data solution. These are rare and predictably located, hugging the mean-conditional ceiling at the very top of the umbrella; certifying them requires full enumeration (e.g. the `unsum` CLOSURE algorithm). See the validation document in `validation/`.

## TODO

- **CRAN check time is inflated by the current CRAN release of `scrutiny`.** Note the direction: it is CRAN's **0.6.1** that is slow, not the GitHub main branch. GRIMMER on 0.6.1 evaluates roughly **25x slower** than on main (0.6.2) — 200 evaluations take 1.78s vs 0.31s, and `umbrella_data(n = 12, l = 1, u = 7, digits = 2)` takes 113s vs 4s. GRIM and the `round_*` helpers are unaffected. Because 0.6.1 is what CRAN builds against, anything GRIMMER-heavy is expensive there:
    - Examples have been sized against 0.6.1 and now total ~3.3s, but the test suite still exceeds 10 minutes on it. The remaining hotspots are the `umbrella_data(n = 12, l = 1, u = 7, digits = 2)` call in `tests/testthat/test-builders-and-plots.R` and the `"integer"` / `"integer_alpha"` rules at `digits = 2` in `tests/testthat/test-plot_sd_region.R`. Shrink where coverage survives, `skip_on_cran()` where it does not.
    - 0.6.1 also warns on every `grimmer()` call about a known false-positive in its test 3 ([scrutiny#80](https://github.com/lhdjung/scrutiny/issues/80)), which is worth resolving before relying on GRIMMER verdicts.
    - `R/scrutiny-compat.R` dispatches between the two interfaces at run time, so both versions work; revisit once 0.6.2 reaches CRAN, when the shim and this whole issue can be retired in favour of `Imports: scrutiny (>= 0.6.2)`.

- **Revisit the phrasing of the inconsistency decisions.** `consistent` is a single Boolean over tests with very different epistemic status, and the wording should probably reflect that:
    - `in_scale_range` and `bounds` failures are *arithmetic proofs of impossibility* — no dataset with those summary statistics exists.
    - `grim` and `grimmer` are deferred third-party verdicts with documented false-positive cases, which is why a GRIMMER-only failure already earns a caveat in `note`.
    - `feasibility` is now a residual, meaning "no sample exists for a reason none of the named tests accounts for" — accurate but uninformative to a reader.
    Calling all of these "inconsistent" flattens a proof and a flag into one word. Consider distinguishing *impossible* from *flagged*, deciding whether `failed_tests` tokens should be the test names (`brim`, `brimmer`, `grim`, `grimmer`) rather than the current mixture of column names and concepts (`in_scale_range`, `bounds`), and settling how the verdict should be phrased in a report a reader may act on. Worth fixing before CRAN pins the return shape.

## Suggested citation

Hussey, I. (2024). strait: Bounds checks for reported summary statistics. https://github.com/ianhussey/strait doi: [10.5281/zenodo.21439905](https://doi.org/10.5281/zenodo.21439905)

## References

- N. J. L. Brown and J. A. J. Heathers (2017), "The GRIM test: A simple technique detects numerous anomalies in the reporting of results in psychology," *Social Psychological and Personality Science*, 8(4):363–369. [doi:10.1177/1948550616673876](https://doi.org/10.1177/1948550616673876)

## License

Code is MIT licensed © Ian Hussey.
