
tides <- function(mean, sd, n, min, max, verbose = TRUE){
  
  require(tibble)
  require(dplyr)
  
  # handle edge cases where the mean is at the minimum or maximum
  if (mean == min || mean == max) {
    max_sd <- 0
  } else {
    
    # mirror values for special cases to ensure min < max
    if (abs(min) > abs(max)) {
      temp <- min
      min <- max
      max <- temp
    }
    
    # calculate the number of observations at the maximum value
    num_max <- floor((n * mean - n * min) / (max - min))
    
    # calculate the number of observations at the minimum value
    num_min <- n - 1 - num_max
    
    # adjust max if num_max is 0
    if (num_max == 0) {
      max <- 0
    }
    
    # compute the sum of means adjusted by the number of min and max values
    adjusted_mean_sum <- n * mean - num_min * min - num_max * max
    
    # compute maximum variability
    max_variability <- (num_min * (min - mean)^2 + num_max * (max - mean)^2 + (mean - adjusted_mean_sum)^2) / (n - 1)
    
    # calculate the maximum standard deviation
    max_sd <- sqrt(max_variability)
  }
  
  # standardize the mean and SD based on the max possible range
  standardized_mean <- (mean - min) / (max - min)
  max_standardized_sd <- ( sqrt( (standardized_mean) * (1 - (standardized_mean)) ) ) * ( sqrt(n / (n - 1)) )
  standardized_sd <- sd / max_sd
  
  # create the result tibble based on verbosity
  if (verbose) {
    res <- tibble(mean = mean,
                  sd = sd,
                  n = n,
                  min = min,
                  max = max,
                  standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd ~ "Inconsistent",
                                     sd <= max_sd ~ "Consistent"))
  } else if (!verbose){
    res <- tibble(standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd ~ "Inconsistent",
                                     sd <= max_sd ~ "Consistent"))
  }
  
  return(res)
}
