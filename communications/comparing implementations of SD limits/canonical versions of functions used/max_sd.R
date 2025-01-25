max_sd <- function(n, min_score, max_score, mean, integer_responses = TRUE) {
  # 1) required sum
  required_sum <- n * mean
  
  # check: is required_sum in feasible range [n*min_score, n*max_score]?
  if (required_sum < n*min_score - 1e-9 || required_sum > n*max_score + 1e-9) {
    stop("No integer distribution can achieve this mean with responses in [min_score,max_score].")
  }
  
  # round required_sum if the problem implies the mean is exactly 
  # representable by integer responses:
  if(integer_responses){
    required_sum <- round(required_sum)
  }
  
  # 2) solve for x 
  x <- floor((required_sum - n*min_score) / (max_score - min_score))
  x <- max(0, min(x, n))  # keep it in [0, n]
  
  # 3) leftover = required_sum - [x*max_score + (n-x)*min_score]
  delta <- required_sum - (x*max_score + (n - x)*min_score)
  if (delta < 0 || delta >= (max_score - min_score)) {
    stop("Something went wrong with leftover. Check input.")
  }
  
  # 4) sum of squares around mean:
  
  # part A: from x responses at max_score
  ss_max_score <- x * (max_score - mean)^2
  
  # part B: from (n - x - 1) responses at min_score (if we do have a leftover) 
  # or from (n - x) responses at min_score (if no leftover)
  if (delta == 0) {
    # no middle value
    ss_min_score <- (n - x) * (min_score - mean)^2
    ss_m <- 0
  } else {
    ss_min_score <- (n - x - 1) * (min_score - mean)^2
    middle_val <- min_score + delta
    ss_m <- (middle_val - mean)^2
  }
  
  # total sum of squares
  ss_total <- ss_max_score + ss_min_score + ss_m
  
  # sample variance = ss_total / (n - 1)
  var_max <- ss_total / (n - 1)
  sd_max  <- sqrt(var_max)
  
  data.frame(max_sd = sd_max)
}