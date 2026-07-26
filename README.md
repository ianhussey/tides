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

### Exact certification

The tests above are *necessary* — failing one proves a report impossible — but not *sufficient*. A small residual set of tuples clears all of them and still has no integer solution.

`certify()` settles those exactly, without reconstructing any dataset. It enumerates the exact attainable `(mean, sd)` lattice for a design by dynamic programming over the reachable *(sum, sum of squares)* states, rounds it to the reporting precision, and tests membership:

```r
# passes the bounds, GRIM and GRIMMER, yet no integer sample produces it
certify(l = 1, u = 5, n = 9, mean = 1.3, sd = 0.9, digits = 1)
#>   mean  sd possible rules
#> 1  1.3 0.9    FALSE
```

This is the same certificate the CLOSURE algorithm provides (`unsum::closure_generate()`), reached analytically rather than by search — an **analytic CLOSURE certification**. Verified cell-for-cell against CLOSURE across six designs (~5,700 tuples, zero disagreements) at roughly **700–1000x** the speed, because the cost depends only on `l`, `u` and `n` rather than on how many datasets satisfy the constraints, and one lattice certifies every tuple of a design at once.

The trade is that no witness datasets are produced. Use CLOSURE when you need the actual candidate samples; use `certify()` when the verdict is the deliverable. See `vignette("certification")`.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ianhussey/strait")
```

## Usage

See also the vignettes in the R package: `vignette("strait")` for the bounds
and the consistency verdict, `vignette("certification")` for exact certification
and how it compares with CLOSURE.

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
| `certify(l, u, n, mean, sd, digits, ...)` | exact possible / impossible certificate for reported tuples, by analytic enumeration of the attainable lattice (no dataset reconstruction) |
| `sd_bounds_curve(l, u, n, ...)` | trace the floor and ceiling of the SD across the mean (hole-free under `"quasiinteger"`) |
| `umbrella_data(n, l, u, ...)` | build the grid of reported (mean, SD) pairs with their consistency verdicts |
| `plot_sd_bounds(curve, ...)` | plot the SD-bounds envelope on the native scale, with reported points |
| `plot_sd_bounds_pomp(curve, ...)` | plot on a POMP scale (`reference = "parity"` or `"sharp"`) so designs can be pooled |
| `plot_umbrella(umbrella, ...)` | plot the feasible (mean, SD) umbrella |

The single-purpose bound primitives (e.g. `sd_max_structure_s()`, `sd_min_quasi_integer()`, `unround_interval()`) are also exported.

## Limitations

- **A small residual blind spot at the umbrella's edge.** A handful of reported (mean, SD) tuples pass GRIM, GRIMMER *and* the SD bounds yet still have no integer-data solution. These are rare and predictably located, hugging the mean-conditional ceiling at the very top of the umbrella. The closed-form screen is *necessary but not sufficient*: it never rejects a report real integer data can produce, but it does admit these. `certify()` settles them exactly — see below. See also the validation document in `validation/`.

## TODO

- **Do not submit to CRAN until `scrutiny` 0.6.2 is released. The current CRAN release returns wrong GRIMMER verdicts.** This is a correctness problem, not merely a slow one. Note the direction throughout: it is CRAN's **0.6.1** that is affected, not the GitHub main branch.

    **The defect.** scrutiny 0.6.1's GRIMMER test 3 flags attainable values as inconsistent ([scrutiny#80](https://github.com/lhdjung/scrutiny/issues/80); it warns about this on every call). Because CRAN builds against 0.6.1, a CRAN release of `strait` would ship those false flags. On a 0–6 scale at *n* = 12 and one decimal place, 24 of 1497 grid cells differ between the two scrutiny versions — **all in the same direction**, 0.6.1 rejecting what 0.6.2 accepts, and all of them `in_bounds`, so GRIMMER alone is responsible. That is roughly a **2% false-flag rate** on legitimate reports. For a tool used to question published work, a false impossibility is the costly error.

    **`certify()` proves 0.6.1 is the wrong one.** This is no longer an inference from two versions disagreeing. `certify()` enumerates the attainable lattice constructively, and at *n* = 12 on 1–5 it proves 16 tuples attainable that 0.6.1 rejects. One of them, mean 1.2 / SD 0.5, has the explicit witness `c(rep(1, 9), 2, 2, 2)` — twelve integers in range, exact mean 1.25 → "1.2", exact SD 0.4523 → "0.5" — which `unsum::closure_generate()` independently confirms with one solution.

    **Three tests already detect it**, and none should be marked `skip_on_cran()`, since they are the only thing catching the upstream bug: `test-plot_sd_region.R:226` (forward-rounded attainable tuples must be a subset of the GRIMMER lattice), `test-plot_sd_region.R:237` (grid consistency count), and `test-certify.R:30` (the screen must never reject what `certify()` proves attainable). The last is the sharpest, being a contradiction of a constructive proof rather than a disagreement between heuristics.

    **If a release cannot wait**, the defensive option is for `.grimmer_compat()` to detect the affected scrutiny and return `NA` rather than propagate a wrong `FALSE` — GRIMMER simply unavailable on old scrutiny, which is honest, leaving the bounds tests and `certify()` fully functional. That is a deliberate design decision, not a workaround to apply silently.

    **Timing, secondarily.** GRIMMER on 0.6.1 also evaluates roughly **25x slower** than on 0.6.2 — 200 evaluations take 1.78s vs 0.31s, and `umbrella_data(n = 12, l = 1, u = 7, digits = 2)` takes 113s vs 4s. GRIM, the `round_*` helpers and `certify()` are unaffected. Examples have been sized against 0.6.1 and total ~3.5s, and both vignettes rebuild in ~142s, but the test suite runs ~520s there and emits ~39,000 warnings. The remaining hotspots are the `umbrella_data(n = 12, l = 1, u = 7, digits = 2)` call in `tests/testthat/test-builders-and-plots.R` and the `"integer"` / `"integer_alpha"` rules at `digits = 2` in `tests/testthat/test-plot_sd_region.R`.

    **Exit condition.** `R/scrutiny-compat.R` dispatches between the two argument interfaces at run time, so both versions run. Once 0.6.2 reaches CRAN, the shim, the timing problem and the correctness problem all retire together in favour of `Imports: scrutiny (>= 0.6.2)`. Worth asking the `scrutiny` maintainer for that timeline.

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
