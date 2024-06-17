min_sd <- function(n, mean) {
  # determine the closest integer values around the mean
  lower_value <- floor(mean)
  upper_value <- ceiling(mean)

  # calculate frequencies required to achieve the mean
  total_sum <- n * mean
  lower_count <- n * (upper_value - mean)
  upper_count <- n * (mean - lower_value)

  # if the calculated counts are not integers, round them appropriately
  lower_count <- round(lower_count)
  upper_count <- round(upper_count)

  # ensure the total count equals n
  if (lower_count + upper_count != n) {
    lower_count <- n - upper_count
  }

  # form the combination of values
  values <- c(rep(lower_value, lower_count), rep(upper_value, upper_count))

  # calculate the mean and standard deviation
  actual_mean <- mean(values)
  min_sd <- sd(values)

  return(list(actual_mean = actual_mean, min_sd = min_sd))
}


# Function to check the consistency of the reported SD
tides <- function(mean, sd, n, min, max, verbose = TRUE){
  
  require(tibble)
  require(dplyr)
  
  # Handle edge cases where the mean is at the minimum or maximum
  if (mean == min || mean == max) {
    max_sd <- 0
    min_sd <- 0
  } else {
    
    # Mirror values for special cases to ensure min < max
    if (abs(min) > abs(max)) {
      temp <- min
      min <- max
      max <- temp
    }
    
    # Calculate the number of observations at the maximum value
    num_max <- floor((n * mean - n * min) / (max - min))
    
    # Calculate the number of observations at the minimum value
    num_min <- n - 1 - num_max
    
    # Adjust max if num_max is 0
    if (num_max == 0) {
      max <- 0
    }
    
    # Compute the sum of means adjusted by the number of min and max values
    adjusted_mean_sum <- n * mean - num_min * min - num_max * max
    
    # Compute maximum variability
    max_variability <- (num_min * (min - mean)^2 + num_max * (max - mean)^2 + (mean - adjusted_mean_sum)^2) / (n - 1)
    
    # Calculate the maximum standard deviation
    max_sd <- sqrt(max_variability)
    
    # Calculate the minimum standard deviation using the provided function
    min_sd <- min_sd(n, mean)$min_sd
  }
  
  # Standardize the mean and SD based on the max possible range
  standardized_mean <- (mean - min) / (max - min)
  max_standardized_sd <- ( sqrt( (standardized_mean) * (1 - (standardized_mean)) ) ) * ( sqrt(n / (n - 1)) )
  standardized_sd <- sd / max_sd
  
  # Create the result tibble based on verbosity
  if (verbose) {
    res <- tibble(mean = mean,
                  sd = sd,
                  n = n,
                  min = min,
                  max = max,
                  standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | sd < min_sd ~ "Inconsistent",
                                     sd <= max_sd & sd >= min_sd ~ "Consistent"))
  } else if (!verbose){
    res <- tibble(standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | sd < min_sd ~ "Inconsistent",
                                     sd <= max_sd & sd >= min_sd ~ "Consistent"))
  }
  
  return(res)
}
