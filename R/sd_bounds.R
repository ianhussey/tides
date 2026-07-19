#' Compute Theoretical SD Bounds for a Given Mean and Range
#'
#' Given a reported mean, sample size, and known minimum/maximum possible values
#' (with optional discretization granularity), this function returns the
#' smallest and largest standard deviations that could produce that mean
#' under those constraints.
#'
#' The lower bound (min_sd) is computed only when \code{calculate_min_sd = TRUE},
#' otherwise it is set to zero (the trivial case of all observations equal the mean).
#' The upper bound (max_sd) is always computed. Rounding precision is inferred
#' from the decimals in \code{mean} unless \code{digits} is specified.
#'
#' @param mean   Numeric. The reported sample mean.
#' @param n      Integer. The sample size.
#' @param min    Numeric. The minimum possible (or observed) value.
#' @param max    Numeric. The maximum possible (or observed) value.
#' @param n_items  Integer(1). Number of discrete “items” averaged at participant level
#'                (e.g.\ number of Likert items). Defaults to 1 (no within-participant averaging).
#' @param digits   Integer or \code{NULL}. Number of decimal places used when
#'                 comparing means or rounding SD. If \code{NULL}, it is inferred
#'                 from the decimal precision of \code{mean}.
#' @param calculate_min_sd  Logical. If \code{TRUE}, computes the minimum
#'                 achievable SD given the constraints; if \code{FALSE}, sets
#'                 \code{min_sd = 0} without computation. Defaults to \code{TRUE}.
#' @param return_distributions Logical. If \code{TRUE}, returns the vectors that
#'                 achieve the min and max SDs in columns `min_dist` and `max_dist`.
#'
#' @return A single row data.frame with the columns min_sd and max_sd:  
#'   \item{min_sd}{Minimum feasible standard deviation (or 0 if \code{calculate_min_sd=FALSE}).}  
#'   \item{max_sd}{Maximum feasible standard deviation.}
#'
#' @examples
#' # A 5-point scale (1-5), mean = 3.2, n = 30, no within-person averaging:
#' sd_bounds(mean = 3.2, n = 30, min = 1, max = 5)
#'
#' # If you only care about the upper bound:
#' sd_bounds(mean = 3.2, n = 30, min = 1, max = 5, calculate_min_sd = FALSE)
#'
#' @importFrom janitor round_half_up
#'
#' @export
sd_bounds <- function(mean, n, min, max, n_items = 1, digits = NULL,
                      calculate_min_sd = TRUE, return_distributions = FALSE) {
  
  # 1. infer precision if not specified
  if (is.null(digits)) {
    digits <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  } 
  
  # 2. precompute alphas, betas, and total
  min_alpha <- min
  max_alpha <- floor(mean * n_items) / n_items
  max_beta  <- min(max(max, min + 1, max_alpha + 1), max)
  min_beta  <- min(max_alpha + 1 / n_items, max)
  total     <- janitor::round_half_up(mean * n * n_items) / n_items
  
  # # construct all possible discrete values - original loop method, slow
  # poss_values <- max
  # for (i in seq_len(n_items)) {
  #   poss_values <- c(poss_values, min:(max-1) + (1 / n_items) * (i - 1))
  # }
  # poss_values <- sort(poss_values)
  
  # 3. build the discrete grid of possible values
  a <- min
  b <- max
  base_seq <- seq(a, b - 1)
  offsets  <- (0:(n_items - 1)) / n_items
  poss_values <- sort(as.vector(outer(base_seq, offsets, `+`)))
  poss_values <- c(poss_values, b)
  
  # 4. initialize result:
  #    - if calculate_min_sd=FALSE, we know lower bound should be 0
  #    - otherwise leave as NA until we compute it
  # we will fill result[1]=min_sd, result[2]=max_sd
  # if min is not calculated, then it is zero as SD must be non-negative
  result <- c(
    if (!calculate_min_sd) 0 else NA_real_,
    NA_real_
  )
  
  # store vectors used to calculate SDs (optional)
  distributions <- list(min_dist = NULL, max_dist = NULL)
  
  # 5. set up the scenarios to run
  scenarios <- list(
    # always compute the upper‐bound of SD (idx = 2)
    list(a = min_alpha, b = max_beta, idx = 2L, name = "max_dist")
  )
  if (calculate_min_sd) {
    # only include the lower‐bound scenario if requested
    scenarios <- c(
      list(list(a = max_alpha, b = min_beta, idx = 1L, name = "min_dist")),
      scenarios
    )
  }
  
  # 6. loop over whichever scenarios we’ve decided to run
  # note that in the loop, `a` and `b` only refer to scenario-specific bounds, not the values set above
  for (sc in scenarios) {
    a <- sc$a; b <- sc$b; m <- sc$idx; name <- sc$name
    # clamp to [min, max]
    a <- min(max(a, min), max)
    b <- min(max(b, min), max)
    
    if (a == b) {
      vec <- rep(a, n)
    } else {
      # number of times to use 'a' vs 'b'
      k    <- round((total - n * b) / (a - b))
      k    <- min(max(k, 1), n - 1)
      vec  <- c(rep(a, k), rep(b, n - k))
      diff <- sum(vec) - total
      
      if (diff < 0) {
        vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n - k))
      } else if (diff > 0) {
        vec <- c(rep(a, k), b - diff, rep(b, n - k - 1))
      }
    }
    
    # only store if the rounded mean matches and values are feasible
    if (janitor::round_half_up(mean(vec), digits) == janitor::round_half_up(mean, digits) &&
        all(floor(vec * 10e9) %in% floor(poss_values * 10e9))) {
      result[m] <- janitor::round_half_up(sd(vec), digits)
      distributions[[name]] <- vec
    }
  }
  
  result_df <- data.frame(min_sd = result[1],
                          max_sd = result[2])
  
  if (return_distributions) {
    result_df$min_dist <- list(distributions$min_dist)
    result_df$max_dist <- list(distributions$max_dist)
  }
  
  return(result_df)
}
