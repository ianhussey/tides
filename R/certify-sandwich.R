# Fast-path certification for brimmest(): decide one reported tuple by
# arithmetic and a pruned constructive search, rather than by sweeping a state
# space whose size is set by the design.
#
# Both existing routes -- the full attainable lattice and the targeted
# corridor DP in R/attainable-target.R -- answer "which states are reachable?"
# and then read the answer off. That is the right shape for a grid of reports
# and the wrong shape for one. This file answers the membership question
# directly, in three layers of increasing cost, and hands back to the corridor
# DP only when the third is cut short.
#
# Everything here works on the shifted integer scale y = mg * (x - l), so
# y is a whole number in [0, W] with W = mg * (u - l), and
#
#   S = sum(y)      the sample sum, pinned to a few integers by the mean
#   Q = sum(y^2)    the sum of squares, pinned to a window by the SD
#
# Layer 1 -- the sandwich screen, O(1) per candidate sum. For a given S the
# achievable Q values lie between the clustered configuration (.q_min_int,
# every value on the two integers around the mean) and the Structure-S
# configuration (.q_max_int, as many values as possible at the top of the
# scale). They also share S's parity, because y^2 = y (mod 2) makes
# Q = S (mod 2) for every sample. A window that meets no integer of the right
# parity inside that sandwich is impossible, decided in microseconds.
#
# Layer 2 -- a constructive search over non-increasing value sequences, which
# is to say over the partitions of S into at most n parts of size at most W.
# Each node re-applies the layer-1 sandwich to what is left to place, which
# prunes hard: near either wall of the scale the surviving tree is a handful
# of nodes, so those cells (where the closed-form screens leak, and where the
# known blind-spot cells sit) are settled outright.
#
# Layer 3 -- the same search run to exhaustion. Reaching a leaf is a proof of
# possibility and yields a witness sample; exhausting the tree is a proof of
# impossibility. Only when neither happens within the node budget is a verdict
# withheld, and then the corridor DP supplies it.
#
# Two structural choices keep the tree small. Requiring the sequence to be
# non-increasing collapses the n! orderings of a sample to one, and reflecting
# y -> W - y whenever the sum sits above the midpoint means the search always
# builds from the nearer wall, where the partition tree is shallow. A third
# keeps it usable: the search is depth-first on an explicit stack rather than
# recursive, since it is one level deep per observation placed and a sample
# size in the thousands would otherwise exhaust R's evaluation depth long
# before the node budget bit.

# Internal: the smallest sum of squares of n non-negative integers summing to
# S -- the clustered configuration, values split between floor(S/n) and one
# more. This is the integer form of the sd_min_integer() floor.
.q_min_int <- function(S, n) {
  m <- S %/% n
  r <- S - n * m
  (n - r) * m * m + r * (m + 1) * (m + 1)
}

# Internal: the largest sum of squares of integers in [0, cap] summing to S --
# the Structure-S configuration, as many values as possible at cap, one
# remainder, the rest at 0. The integer form of sd_max_structure_s().
# A cap of 0 forces S = 0, hence a sum of squares of 0.
.q_max_int <- function(S, cap) {
  cap <- rep(cap, length.out = length(S))
  out <- numeric(length(S))
  pos <- cap > 0
  if (any(pos)) {
    cp <- cap[pos]
    sp <- S[pos]
    nu <- sp %/% cp
    rem <- sp - nu * cp
    out[pos] <- nu * cp * cp + rem * rem
  }
  out
}

# Internal: layer 1. Narrow one candidate's Q window to the integers that
# could actually occur -- inside the sandwich and of the right parity.
# Returns c(k_lo, k_hi) or NULL when the candidate is already impossible.
#
# The window arrives as the real interval implied by the reported SD, with
# endpoint inclusion flags, so an endpoint that the rounding rule excludes
# must not be admitted even when it lands exactly on an integer.
.k_window <- function(S, Q_lo, Q_hi, lo_incl, hi_incl, n, W, tol = 1e-9) {
  k_lo <- ceiling(Q_lo - tol)
  if (!lo_incl && abs(k_lo - Q_lo) < tol) k_lo <- k_lo + 1
  k_hi <- floor(Q_hi + tol)
  if (!hi_incl && abs(k_hi - Q_hi) < tol) k_hi <- k_hi - 1
  k_lo <- max(k_lo, .q_min_int(S, n))
  k_hi <- min(k_hi, .q_max_int(S, W))
  if ((k_lo %% 2) != (S %% 2)) k_lo <- k_lo + 1
  if ((k_hi %% 2) != (S %% 2)) k_hi <- k_hi - 1
  if (k_lo > k_hi) return(NULL)
  c(k_lo, k_hi)
}

# Internal: layers 2 and 3. Is there a sample of n integers in [0, W] with sum
# S and sum of squares in [k_lo, k_hi]?
#
# Returns a list with `possible` (TRUE, FALSE, or NA when the node budget was
# reached before the tree was exhausted), `witness` (a non-increasing integer
# vector on the shifted scale when possible), and `nodes` (the search cost).
.witness_search <- function(W, n, S, k_lo, k_hi, budget = 2e5) {
  # Build from the nearer wall. Under y -> W - y the sum becomes n*W - S and
  # the sum of squares shifts by n*W^2 - 2*W*S, so the window travels with it.
  flip <- 2 * S > n * W
  if (flip) {
    shift <- n * W * W - 2 * W * S
    S <- n * W - S
    k_lo <- k_lo + shift
    k_hi <- k_hi + shift
  }

  # The next value to place, given that `i` remain, none may exceed `cap`,
  # they must sum to S, and their squares must sum into [k_lo, k_hi].
  # Returns the admissible choices, best first, or an empty vector when the
  # state is already impossible.
  #
  # Note that when i == 1 this is the leaf test: the single remaining value is
  # forced, and it survives the filter only if it lands the sum and the sum of
  # squares exactly. So an admissible choice at i == 1 is a complete sample.
  children <- function(i, cap, S, k_lo, k_hi) {
    # The i - 1 values after this one are all <= y, so y must be at least
    # S / i; and it cannot exceed the running cap or the sum itself.
    y_hi <- min(cap, S)
    y_lo <- (S + i - 1) %/% i          # ceiling(S / i), without leaving integers
    if (y_lo > y_hi) return(numeric(0))
    ys <- y_lo:y_hi

    j <- i - 1
    Sp <- S - ys
    kl <- k_lo - ys * ys
    kh <- k_hi - ys * ys
    if (j == 0) return(ys[Sp == 0 & kl <= 0 & kh >= 0])

    qm <- .q_min_int(Sp, j)
    qM <- .q_max_int(Sp, ys)
    lo <- pmax(kl, qm)
    hi <- pmin(kh, qM)
    lo <- lo + ((lo %% 2) != (Sp %% 2))
    ok <- Sp >= 0 & Sp <= j * ys & lo <= hi
    if (!any(ok)) return(numeric(0))
    # Order the children by how centrally the surviving window sits inside the
    # child's own sandwich. A target hugging either end of what its subtree
    # can reach is the one likeliest to need backtracking, so trying the
    # roomiest child first is what turns the search into a straight dive for
    # the overwhelming majority of attainable reports.
    wid <- qM - qm
    pos <- ifelse(wid > 0, ((lo + hi) / 2 - qm) / wid, 0.5)
    ys[ok][order(abs(pos - 0.5)[ok])]
  }

  # Depth-first, on an explicit stack rather than by recursion: the tree is n
  # deep, and a sample size in the thousands would otherwise exhaust R's
  # evaluation depth long before the node budget bit.
  f_i <- numeric(n); f_S <- numeric(n)
  f_lo <- numeric(n); f_hi <- numeric(n); f_at <- integer(n)
  kids <- vector("list", n)
  acc <- numeric(n)

  d <- 1L
  f_i[1] <- n; f_S[1] <- S; f_lo[1] <- k_lo; f_hi[1] <- k_hi; f_at[1] <- 1L
  kids[[1]] <- children(n, W, S, k_lo, k_hi)

  nodes <- 0
  capped <- FALSE
  hit <- NULL
  while (d > 0L) {
    if (nodes >= budget) {
      capped <- TRUE
      break
    }
    at <- f_at[d]
    cd <- kids[[d]]
    if (at > length(cd)) {                  # this subtree is exhausted
      d <- d - 1L
      next
    }
    f_at[d] <- at + 1L
    y <- cd[at]
    acc[d] <- y
    nodes <- nodes + 1
    if (f_i[d] == 1) {                      # every value placed, window met
      hit <- acc
      break
    }
    ni <- f_i[d] - 1
    nS <- f_S[d] - y
    nlo <- f_lo[d] - y * y
    nhi <- f_hi[d] - y * y
    nc <- children(ni, y, nS, nlo, nhi)
    if (length(nc)) {
      d <- d + 1L
      f_i[d] <- ni; f_S[d] <- nS; f_lo[d] <- nlo; f_hi[d] <- nhi
      f_at[d] <- 1L; kids[[d]] <- nc
    }
  }

  if (!is.null(hit)) {
    if (flip) hit <- W - hit
    return(list(possible = TRUE, witness = hit, nodes = nodes))
  }
  list(possible = if (capped) NA else FALSE, witness = NULL, nodes = nodes)
}

# Internal: the fast route for one reported tuple under one rounding rule.
# Takes the (S, Q-window) targets .target_states() already builds, so the
# endpoint semantics per rounding rule are shared with the corridor DP rather
# than reimplemented.
#
# Returns TRUE / FALSE / NA, with NA meaning "no verdict within budget" --
# the caller then falls back to .attainable_target().
.certify_fast <- function(W, n, tg, budget = 2e5) {
  if (is.null(tg) || !nrow(tg)) return(FALSE)
  unknown <- FALSE
  for (i in seq_len(nrow(tg))) {
    kw <- .k_window(tg$S[i], tg$Q_lo[i], tg$Q_hi[i],
                    tg$lo_incl[i], tg$hi_incl[i], n, W)
    if (is.null(kw)) next                   # layer 1 settles this candidate
    res <- .witness_search(W, n, tg$S[i], kw[1], kw[2], budget = budget)
    if (isTRUE(res$possible)) return(TRUE)
    if (is.na(res$possible)) unknown <- TRUE
  }
  if (unknown) NA else FALSE
}
