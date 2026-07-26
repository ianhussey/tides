# Exact certification of reported (mean, SD) tuples against the attainable
# lattice of strictly integer data. See R/plot_sd_region.R for the dynamic
# program that enumerates the lattice.

#' Certify whether a reported (mean, SD) is attainable by integer data
#'
#' An exact possible / impossible certificate for reported summary statistics
#' on a bounded integer scale, obtained without reconstructing any dataset.
#'
#' [sd_bounds()] and [brimmer()] answer a *necessary* question — does the
#' reported SD lie inside the feasible band? — and a handful of tuples pass
#' that, GRIM and GRIMMER, yet still have no integer solution. `certify()`
#' settles those: it enumerates the exact attainable `(mean, sd)` lattice for
#' the design by dynamic programming over the reachable
#' `(sum, sum of squares)` states, rounds that lattice to the reporting
#' precision, and asks whether the reported tuple is a member.
#'
#' The verdict is exact in both directions. A hit means some integer sample on
#' `[l, u]` of size `n` rounds to exactly this report, so the report is
#' **possible**. A miss means no such sample exists, so the report is
#' **impossible** — a proof, not a heuristic flag.
#'
#' This is the same question the CLOSURE algorithm answers (see
#' `unsum::closure_generate()`), reached analytically rather than by search —
#' an *analytic CLOSURE certification*. The trade is that no witness datasets
#' are produced: the lattice records which `(mean, sd)` pairs are reachable,
#' not how. In exchange the cost depends only on `l`, `u` and `n`, not on how
#' many datasets happen to satisfy the constraints, and one lattice certifies
#' every tuple of that design at once — so a vector of reports costs barely
#' more than a single one. See `vignette("certification")`.
#'
#' @section Rounding and the direction of proof:
#' A hit certifies possibility under whichever rounding rule produced it. A
#' miss certifies impossibility only *relative to the rules in `rounding`*,
#' because a report unreachable under one convention may be reachable under
#' another. The default takes the union of `"half_up"` and `"half_down"`,
#' which spans the usual ambiguity about how a source rounded its halves.
#' Narrow it only when the source's convention is actually known: certifying
#' against one rule when the paper used another manufactures a false
#' impossibility. Note that unlike [brimmer()] there is no `"up_or_down"`
#' option, since rounding the lattice forward needs a definite direction.
#'
#' @param l,u Numeric scalars, the scale limits. Integer-valued (in mean-score
#'   units when `n_items > 1` and `scoring` is `"meanscored"`).
#' @param n Integer scalar, sample size.
#' @param mean,sd Numeric vectors of reported means and SDs, recycled against
#'   each other. All are certified against one lattice.
#' @param digits Integer, the reported decimal places, used for both `mean`
#'   and `sd` unless the next two are given.
#' @param mean_digits,sd_digits Integer scalars or NULL; reported decimal
#'   places when the mean and SD are reported to different precision.
#' @param rounding Character vector of rounding rules to admit, from
#'   `"half_up"`, `"half_down"`, `"native"`, `"ceiling"`, `"floor"`,
#'   `"trunc"`, `"anti_trunc"`. A tuple is possible if it is reachable under
#'   any of them.
#' @param scoring "singleitem" (default), "sumscored", or "meanscored".
#' @param n_items Positive whole number of response items (default 1).
#' @param max_cells Guard on the dynamic program's state space (default
#'   `2e7`); the enumeration errors rather than exhausting memory above it.
#' @return A data.frame with one row per reported tuple: `mean`, `sd`,
#'   `possible` (logical), and `rules` — the rounding rules under which the
#'   tuple is reachable, comma-separated and `""` when none.
#' @seealso [brimmer()] for the closed-form screen to run first, and
#'   [sd_region_data()] for the lattice itself.
#' @examples
#' # a report a real 1-5 scale sample can produce
#' certify(l = 1, u = 5, n = 9, mean = 3.0, sd = 1.0, digits = 1)
#'
#' # inside the SD bounds and passing GRIM and GRIMMER, yet no integer
#' # sample produces it: the residual blind spot of the closed-form screen
#' certify(l = 1, u = 5, n = 9, mean = 1.3, sd = 0.9, digits = 1)
#'
#' # one lattice certifies many reports at once
#' certify(l = 1, u = 5, n = 9, digits = 1,
#'         mean = c(3.0, 1.3, 2.5), sd = c(1.0, 0.9, 1.2))
#' @export
certify <- function(l, u, n, mean, sd, digits = NULL,
                    mean_digits = NULL, sd_digits = NULL,
                    rounding = c("half_up", "half_down"),
                    scoring = c("singleitem", "sumscored", "meanscored"),
                    n_items = 1, max_cells = 2e7) {
  scoring <- match.arg(scoring)
  valid <- c("half_up", "half_down", "native", "ceiling", "floor",
             "trunc", "anti_trunc")
  bad <- setdiff(rounding, valid)
  if (length(bad))
    stop("unknown rounding rule(s): ", paste(bad, collapse = ", "),
         ". Choose from: ", paste(valid, collapse = ", "))
  if (!length(rounding)) stop("at least one rounding rule is required")

  md <- if (!is.null(mean_digits)) mean_digits else digits
  sdd <- if (!is.null(sd_digits)) sd_digits else digits
  if (is.null(md) || is.null(sdd))
    stop("reported decimal places are required: give digits, or both ",
         "mean_digits and sd_digits")
  if (is.null(n) || n < 2) stop("n must be >= 2 for a sample SD")

  k <- as.integer(round(n_items))
  g <- .scoring_geometry(scoring, k, l, u)

  # one lattice per design, reused across every reported tuple
  lat <- .attainable_lattice(l, u, n, g$mg, max_cells = max_cells)

  nn <- max(length(mean), length(sd))
  mean <- rep(mean, length.out = nn)
  sd <- rep(sd, length.out = nn)

  # Both sides are on the 10^-digits reporting grid, so compare them as whole
  # numbers of grid steps rather than as strings: exact, and it keeps a
  # multi-million-row lattice cheap to match against. Integers also sidestep
  # the negative zero some rounding rules return at 0, which compares equal
  # but formats differently.
  mult_m <- 10^md
  mult_s <- 10^sdd
  qm <- round(mean * mult_m)
  qs <- round(sd * mult_s)

  hit_mat <- vapply(rounding, function(rr) {
    lm <- round(.round_reported(lat$mean, md, rr) * mult_m)
    ls <- round(.round_reported(lat$sd, sdd, rr) * mult_s)
    span <- max(c(ls, qs), 0) + 1              # same packing for both sides
    (qm * span + qs) %in% (lm * span + ls)
  }, logical(nn))
  dim(hit_mat) <- c(nn, length(rounding))

  rules <- apply(hit_mat, 1L, function(z) paste(rounding[z], collapse = ","))
  data.frame(mean = mean, sd = sd,
             possible = as.logical(rowSums(hit_mat) > 0),
             rules = rules, stringsAsFactors = FALSE)
}
