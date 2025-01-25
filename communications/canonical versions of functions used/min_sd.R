min_sd <- function(n, min_score, max_score, mean, integer_responses = TRUE) {
  # ---- 1) required total sum (rounded to an integer if the problem implies 
  # exact feasibility)
  required_sum <- n * mean
  
  # round required_sum if the problem implies the mean is exactly representable 
  # by integer responses:
  if(integer_responses){
    required_sum <- round(required_sum)
  }
  
  # feasibility check: the sum must lie between n*min_score and n*max_score 
  # for an integer distribution to exist.
  if (required_sum < n*min_score || required_sum > n*max_score) {
    stop("No distribution of [min_score, max_score] integers can achieve that mean (sum out of range).")
  }
  
  # ---- 2) check if the mean is effectively an integer within [min_score,max_score].
  # if so, the minimal SD is 0 by taking all responses = that integer.
  if (abs(required_sum - n*round(mean)) < 1e-9 && round(mean) >= min_score && round(mean) <= max_score) {
    # i.e. if mean was effectively an integer in the feasible range
    # check if n * round(mean) == required_sum
    if (round(mean) * n == required_sum) {
      # all responses equal to that integer
      dist <- rep(round(mean), n)
      return(data.frame(min_sd = 0))
    }
  }
  
  # ---- 3) otherwise, we attempt to use two adjacent integers L and L+1
  # let L = floor(mean), clamped to [min_score, max_score].
  L_init <- floor(mean)
  if (L_init < min_score) L_init <- min_score
  if (L_init > max_score) L_init <- max_score
  
  # we'll define a small helper to build a distribution given L
  # and return if feasible:
  build_dist_from_L <- function(L, required_sum, n, min_score, max_score) {
    if (L < min_score || L > max_score) {
      return(NULL)
    }
    # leftover = how many times we need (L+1)
    leftover <- required_sum - n*L
    if (leftover == 0) {
      # all L
      return(rep(L, n))
    } else if (leftover > 0 && leftover <= n) {
      # leftover data points (L+1), the rest are L
      # but only if (L+1) <= max_score
      if ((L + 1) <= max_score) {
        return(c(rep(L, n - leftover), rep(L + 1, leftover)))
      } else {
        return(NULL)
      }
    } else {
      # leftover < 0 or leftover > n => not feasible with L and L+1
      return(NULL)
    }
  }
  
  # try L_init directly:
  dist <- build_dist_from_L(L_init, required_sum, n, min_score, max_score)
  if (is.null(dist)) {
    # if that didn't work, try adjusting L_init up or down by 1 step
    # (sometimes floor(mean) is not the right choice if leftover < 0 or > n).
    
    # let's define a small search around L_init:
    candidates <- unique(c(L_init - 1, L_init, L_init + 1))
    candidates <- candidates[candidates >= min_score & candidates <= max_score]
    
    found <- FALSE
    for (L_try in candidates) {
      dist_try <- build_dist_from_L(L_try, required_sum, n, min_score, max_score)
      if (!is.null(dist_try)) {
        dist <- dist_try
        found <- TRUE
        break
      }
    }
    
    if (!found) {
      # possibly no solution with 2 adjacent integers => no feasible distribution
      stop("Could not construct a minimal-variance distribution with two adjacent integers.")
    }
  }
  
  # if we reach here, 'dist' is a valid distribution
  # ---- 4) compute sample SD in R (with denominator n-1)
  min_sd <- sd(dist)
  
  data.frame(min_sd = min_sd)
}