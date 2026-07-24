# Report-level consistency checking (single row and batch) and the data
# builders (bound curves, umbrella grid) that drive the plots.

# ---- Layer 5: report-level consistency check ---------------------------------

#' Check a reported (mean, SD, n) against the SD bounds: consistent or not
#'
#' The report-checking wrapper above [sd_bounds()]: give it the reported
#' summary statistics as they appear in a paper (numeric values plus their
#' reported decimal places), and it unrounds them, computes the bounds
#' envelope, runs the applicable consistency tests, and returns a single
#' consistent/inconsistent verdict with the reasons.
#'
#' Tests applied (each only when its inputs are present):
#' * `feasibility` — does the constraint set admit any sample at all? Under
#'   `Z = "integer"` this includes the GRIM condition that some attainable
#'   mean lies in the reported mean's rounding interval; with attained
#'   extremes it includes the feasible mean band.
#' * `bounds` — does the reported SD's own rounding interval overlap
#'   `[min_sd, max_sd]`? (The package's validated mathematics.)
#' * `grim`, `grimmer` — scrutiny's verdicts, deferred verbatim (only under
#'   `Z = "integer"` with `rounding`). NOTE scrutiny's GRIMMER (and, under
#'   `rounding = "up"`, GRIM) have documented floating-point boundary bugs,
#'   including false flags under the default rounding. When GRIMMER is the ONLY
#'   failing test, the note says so explicitly, since that pattern matches
#'   the documented false-flag family.
#'
#' `consistent` is TRUE iff every applicable test passes. `failed_tests`
#' names the failures (empty string if none).
#'
#' Unlike [sd_bounds()], `rounding` here defaults to `"up_or_down"` — a
#' report-checking context virtually always deals with rounded values. Pass
#' `rounding = NULL` to treat the inputs as exact instead (digits then
#' unused).
#'
#' @param l,u Numeric scalars or NULL, logical limits.
#' @param a,b Numeric scalars or NULL, observed (attained) extremes.
#' @param n Integer scalar, sample size.
#' @param mean Numeric scalar or NULL, the reported mean.
#' @param mean_digits Integer scalar or NULL, its reported decimal places
#'   (required when `mean` and `rounding` are both given).
#' @param sd Numeric scalar, the reported SD (required — it is the thing
#'   being checked).
#' @param sd_digits Integer scalar or NULL, its reported decimal places
#'   (required when `rounding` is given).
#' @param rounding Rounding rule for unrounding the reported values
#'   (default `"up_or_down"`); NULL treats inputs as exact. See
#'   [unround_interval()].
#' @param Z "continuous" (default), "integer", or "quasiinteger".
#' @param scoring "singleitem" (default), "sumscored", or "meanscored".
#' @param n_items Positive whole number of response items (default 1).
#' @param alpha Numeric scalar or NULL, reported Cronbach's alpha.
#'
#' @return A one-row data.frame: `consistent` (logical), `failed_tests`
#'   (comma-separated names, "" if none), then the [sd_bounds()] columns
#'   `min_sd`, `max_sd`, `feasible`, `grim`, `grimmer`, `sd_in_bounds`, the
#'   percent-of-maximum-possible transforms `pomp_mean`, `pomp_sd_parity`
#'   (linear, against the mean-agnostic parity ceiling) and `pomp_sd_sharp`
#'   (position within the sharp mean-conditional band), and `note`.
#'
#' @examples
#' # a perfectly ordinary report on a 1-7 scale
#' # (witnessed by c(rep(1, 20), rep(7, 9), 6): mean 2.9667, sd 2.8343)
#' sd_bounds_check(l = 1, u = 7, n = 30, mean = 2.97, mean_digits = 2,
#'                 sd = 2.83, sd_digits = 2, Z = "integer")
#'
#' # SD below the quasi-integer floor: inconsistent via the bounds test
#' sd_bounds_check(l = 1, u = 7, n = 30, mean = 2.97, mean_digits = 2,
#'                 sd = 0.10, sd_digits = 2, Z = "quasiinteger")
#'
#' # GRIM-impossible mean: inconsistent via feasibility (and grim)
#' sd_bounds_check(l = 1, u = 7, n = 30, mean = 3.51, mean_digits = 2,
#'                 sd = 1.00, sd_digits = 2, Z = "integer")
#' @export
sd_bounds_check <- function(l = NULL, u = NULL, a = NULL, b = NULL,
                            n = NULL, mean = NULL, mean_digits = NULL,
                            sd = NULL, sd_digits = NULL,
                            rounding = "up_or_down",
                            Z = c("continuous", "integer", "quasiinteger"),
                            scoring = c("singleitem", "sumscored", "meanscored"),
                            n_items = 1, alpha = NULL) {
  if (is.null(sd)) stop("sd_bounds_check() checks a reported SD: sd is required")
  if (is.null(n)) stop("sd_bounds_check() requires n")

  # a mean-free check is legitimate (e.g. sd vs the parity ceiling), but
  # rounding then applies to the sd only
  r <- sd_bounds(l = l, u = u, a = a, b = b, n = n,
                 mean = mean, mean_digits = mean_digits,
                 sd = sd, sd_digits = sd_digits,
                 rounding = if (is.null(mean)) NULL else rounding,
                 Z = Z, scoring = scoring, n_items = n_items, alpha = alpha)

  # mean-free path: sd_bounds() skipped unrounding; redo the sd overlap with
  # the sd's own interval if rounding is in force
  if (is.null(mean) && !is.null(rounding)) {
    if (is.null(sd_digits)) stop("rounding requires sd_digits for the reported sd")
    iv <- unround_interval(sd, sd_digits, rounding)
    r$sd_in_bounds <- if (is.na(r$min_sd)) NA else
      (iv$hi >= r$min_sd - 1e-9 && iv$lo <= r$max_sd + 1e-9)
  }

  failed <- character(0)
  if (!isTRUE(r$feasible)) failed <- c(failed, "feasibility")
  if (isFALSE(r$sd_in_bounds)) failed <- c(failed, "bounds")
  if (isFALSE(r$grim)) failed <- c(failed, "grim")
  if (isFALSE(r$grimmer)) failed <- c(failed, "grimmer")

  note <- r$note
  if (identical(failed, "grimmer")) {
    caveat <- "only GRIMMER failed while feasibility and bounds pass; scrutiny's GRIMMER has documented false-flag cases - verify before flagging"
    note <- if (is.na(note)) caveat else paste(note, caveat, sep = "; ")
  }

  # POMP transforms on the reported scale (effective limits: a supersedes l,
  # b supersedes u). Descriptive, so they use the reported point mean/sd, not
  # the unrounding interval.
  eff_lower <- if (!is.null(a)) a else l
  eff_upper <- if (!is.null(b)) b else u
  pomp <- .pomp_cols(mean, sd, r$min_sd, r$max_sd, eff_lower, eff_upper, n,
                     has_mean = !is.null(mean))

  cbind(data.frame(consistent = length(failed) == 0,
                   failed_tests = paste(failed, collapse = ","),
                   stringsAsFactors = FALSE),
        r[, c("min_sd", "max_sd", "feasible", "grim", "grimmer", "sd_in_bounds")],
        data.frame(pomp_mean = pomp$pomp_mean,
                   pomp_sd_parity = pomp$pomp_sd_parity,
                   pomp_sd_sharp = pomp$pomp_sd_sharp,
                   stringsAsFactors = FALSE),
        data.frame(note = note, stringsAsFactors = FALSE))
}

# ---- Layer 6: batch report checking ------------------------------------------

#' Check many reported (mean, SD, n) rows against the SD bounds
#'
#' Applies [sd_bounds_check()] to each row of a data frame, de-duplicating
#' identical constraint tuples so the (possibly expensive) rounding-envelope is
#' computed once per distinct input and reused. Columns of `data` whose names
#' match [sd_bounds_check()] arguments are taken per row; any argument given in
#' `...` is a constant broadcast to every row. Supplying one name both ways is
#' an error.
#'
#' Recognised names (column or constant): `l`, `u`, `a`, `b`, `n`, `mean`,
#' `mean_digits`, `sd`, `sd_digits`, `rounding`, `Z`, `scoring`, `n_items`,
#' `alpha`.
#'
#' @param data A data frame, one reported statistic set per row.
#' @param ... Constant arguments applied to all rows.
#' @param include_inputs If TRUE (default), returns `data` column-bound to the
#'   results; if FALSE, only the result columns (same row order), for drop-in
#'   use inside a `dplyr::mutate()`/`purrr` pipeline.
#' @return A data frame of the [sd_bounds_check()] columns, one row per input
#'   row (optionally with the inputs prepended).
#' @export
sd_bounds_check_multiple <- function(data, ..., include_inputs = TRUE) {
  if (!is.data.frame(data)) stop("data must be a data frame")
  arg_names <- c("l", "u", "a", "b", "n", "mean", "mean_digits", "sd",
                 "sd_digits", "rounding", "Z", "scoring", "n_items", "alpha")
  consts <- list(...)
  unknown <- setdiff(names(consts), arg_names)
  if (length(unknown))
    stop("unknown constant argument(s): ", paste(unknown, collapse = ", "))
  N <- nrow(data)
  resolve <- function(nm) {
    incol <- nm %in% names(data)
    incon <- nm %in% names(consts) && !is.null(consts[[nm]])
    if (incol && incon)
      stop(sprintf("'%s' supplied as both a column and a constant", nm))
    if (incol) data[[nm]]
    else if (incon) rep(consts[[nm]], length.out = N)
    else NULL
  }
  cols <- lapply(arg_names, resolve); names(cols) <- arg_names
  present <- arg_names[!vapply(cols, is.null, logical(1))]
  if (!("sd" %in% present))
    stop("a reported sd is required (as a column of data or a constant)")

  # de-duplicate identical input tuples; compute once per unique tuple
  key <- do.call(paste, c(lapply(present, function(nm)
    format(cols[[nm]], nsmall = 6, trim = TRUE)), sep = "\r"))
  uk_idx <- which(!duplicated(key))
  back <- match(key, key[uk_idx])
  res_uni <- do.call(rbind, lapply(uk_idx, function(i) {
    args <- lapply(present, function(nm) cols[[nm]][i]); names(args) <- present
    do.call(sd_bounds_check, args)
  }))
  res <- res_uni[back, , drop = FALSE]
  rownames(res) <- NULL
  if (include_inputs) cbind(data, res) else res
}

# ---- Layer 7: bound curves, umbrella grid, and plots -------------------------

#' SD-bound curves across the mean (hole-free under granularity)
#'
#' Traces the minimum and maximum sample SD as the mean sweeps `[l, u]`, on a
#' grid dense enough to resolve the piecewise floors and the Structure S ceiling
#' (their kinks fall on the `1/(n * m)` lattice, `m = n_items` for mean-scored
#' data else 1). Under a granularity constraint the quasi-integer floor is
#' defined at every mean, so the curves have no gaps — unlike a strict-integer
#' floor, which exists only at GRIM means. Returns raw bounds and their
#' parity-normalised counterparts for the POMP plot.
#'
#' @param l,u Numeric scalars, limits.
#' @param n Integer scalar, sample size.
#' @param Z granularity; `"quasiinteger"` (default) gives a hole-free floor.
#' @param scoring,n_items,alpha As in [sd_bounds()].
#' @param by Numeric or NULL; mean-grid spacing (default ~ `(u - l) / 1000`).
#' @return A data.frame: `mean`, `min_sd`, `max_sd`, `feasible`, `pomp_mean`,
#'   `parity_max`, `ceil_parity` (= `max_sd / parity_max`), `floor_parity`.
#' @export
sd_bounds_curve <- function(l, u, n, Z = "quasiinteger",
                            scoring = "singleitem", n_items = 1,
                            alpha = NULL, by = NULL) {
  m <- if (scoring == "meanscored") n_items else 1L
  if (is.null(by)) by <- (u - l) / 1000
  nm <- n * m
  kinks <- unique(c(seq(ceiling(l * nm), floor(u * nm)) / nm,
                    seq(ceiling(l * m),  floor(u * m))  / m))
  means <- sort(unique(pmin(u, pmax(l,
    c(seq(l, u, by = by), kinks, kinks + 1e-9, kinks - 1e-9)))))
  out <- do.call(rbind, lapply(means, function(mu) {
    d <- sd_bounds(l = l, u = u, n = n, mean = mu, Z = Z,
                   scoring = scoring, n_items = n_items, alpha = alpha)
    data.frame(mean = mu, min_sd = d$min_sd, max_sd = d$max_sd,
               feasible = d$feasible)
  }))
  parity_max <- sd_max_span_n(l, u, n)
  out$pomp_mean <- (out$mean - l) / (u - l)
  out$parity_max <- parity_max
  out$ceil_parity <- out$max_sd / parity_max
  out$floor_parity <- out$min_sd / parity_max
  out
}

#' Build the GRIM x GRIMMER x bounds umbrella grid (full, with verdicts)
#'
#' For each reported mean on the `10^-digits` grid over `[l, u]`, computes the
#' SD-bounds envelope once, then evaluates every reported SD on the same grid
#' from 0 up to that mean's ceiling, tagging each `(mean, sd)` cell with the
#' bounds-overlap and (under `Z = "integer"`) GRIMMER verdicts. Unlike
#' `tides::umbrella()` this returns the FULL grid — feasible means x candidate
#' SDs — with verdict columns, so the plotting layer can render failures as
#' well as the passing "umbrella".
#'
#' @param n Integer scalar, sample size.
#' @param l,u Numeric scalars, limits.
#' @param digits Integer, reported decimal places (grid step `10^-digits`).
#' @param Z granularity (default `"integer"`).
#' @param scoring,n_items As in [sd_bounds()].
#' @param rounding Rounding rule for mean and SD (default `"up_or_down"`).
#' @return A data.frame: `mean`, `sd`, `min_sd`, `max_sd`, `in_bounds`,
#'   `grimmer`, `consistent`.
#' @export
umbrella_data <- function(n, l, u, digits = 2, Z = "integer",
                          scoring = "singleitem", n_items = 1,
                          rounding = "up_or_down") {
  step <- 10^(-digits)
  h <- step / 2
  means <- seq(l, u, by = step)
  use_grimmer <- Z == "integer" && requireNamespace("scrutiny", quietly = TRUE)
  rows <- list()
  for (mu in means) {
    d <- sd_bounds(l = l, u = u, n = n, mean = mu, mean_digits = digits,
                   rounding = rounding, Z = Z, scoring = scoring,
                   n_items = n_items)
    if (!isTRUE(d$feasible) || is.na(d$max_sd)) next
    sds <- seq(0, ceiling(d$max_sd / step) * step, by = step)
    in_bounds <- (sds + h) >= d$min_sd - 1e-9 & (sds - h) <= d$max_sd + 1e-9
    grimmer <- rep(NA, length(sds))
    if (use_grimmer)
      grimmer <- as.logical(scrutiny::grimmer(
        x = mu, sd = sds, n = n, digits_x = digits, digits_sd = digits,
        items = n_items, rounding = rounding))
    consistent <- in_bounds & (if (use_grimmer) grimmer else TRUE)
    rows[[length(rows) + 1]] <- data.frame(
      mean = mu, sd = sds, min_sd = d$min_sd, max_sd = d$max_sd,
      in_bounds = in_bounds, grimmer = grimmer, consistent = consistent)
  }
  do.call(rbind, rows)
}

# Internal: shared point layer (green/red outlined dots)
