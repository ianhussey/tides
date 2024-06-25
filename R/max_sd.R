#' Calculate maximum Standard Deviation for a truncated variable
#'
#' Explanation to be added
#' 
#' Assumptions to be checked: Does it still work if min or max are not integers?
#' 
#' @param mean numeric variable representing the reported mean.
#' @param sd numeric variable representing the reported Standard Deviation.
#' @param n numeric variable representing the reported sample size.
#' @param min numeric variable representing the variable's minimum possible/observable score.
#' @param max numeric variable representing the variable's maximum possible/observable score.
max_sd <- function(mean, sd, n, min, max){

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
  
  return(max_sd)
}
