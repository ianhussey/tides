calculate_min_sd <- function(N, mean) {
  # Determine the closest integer values around the mean
  lower_value <- floor(mean)
  upper_value <- ceiling(mean)
  
  # Calculate frequencies required to achieve the mean
  total_sum <- N * mean
  lower_count <- N * (upper_value - mean)
  upper_count <- N * (mean - lower_value)
  
  # If the calculated counts are not integers, round them appropriately
  lower_count <- round(lower_count)
  upper_count <- round(upper_count)
  
  # Ensure the total count equals N
  if (lower_count + upper_count != N) {
    lower_count <- N - upper_count
  }
  
  # Form the combination of values
  values <- c(rep(lower_value, lower_count), rep(upper_value, upper_count))
  
  # Calculate the mean and standard deviation
  actual_mean <- mean(values)
  sd_value <- sd(values)
  
  return(list(mean = actual_mean, sd = sd_value))
}

N <- 23
mean_value <- 3.3
calculate_min_sd(N, mean_value)



