#' Calculate minimum Standard Deviation for a truncated and binned variable
#'
#' Explanation to be added
#' 
#' Assumptions to be checked: 
#'  does this assume the mean represents (a) a variable where the possible scores are integers as opposed to something with smaller granularity, and relatedly (b) the mean of a mean mean score rather than the mean of a sum score or single item 
#'  
#' @param mean numeric variable representing the reported mean.
#' @param n numeric variable representing the reported sample size.
#' @returns A numeric variable representing the minimum SD.
#' @export 
min_sd <- function(mean, n) {
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
  #actual_mean <- mean(values) 
  min_sd <- sd(values)
  
  #return(list(actual_mean = actual_mean, min_sd = min_sd))
  return(min_sd)
}

# # alternative weighting approach
# min_sd <- function(mean, n) {
#   # Calculate potential values around the mean
#   base <- floor(mean)
#   values <- base:(base+2)
#   
#   # Minimize the sum of squared differences from the mean
#   results <- expand.grid(rep(list(0:n), length(values)))
#   colnames(results) <- as.character(values)
#   results$sum <- rowSums(results)
#   results <- results[results$sum == n,]
#   results$var <- rowSums((values - mean)^2 * results[,1:length(values)])
#   
#   # Find the configuration with the minimum variance
#   min_var <- min(results$var)
#   optimal <- results[results$var == min_var, 1:length(values)]
#   
#   # Compute standard deviation from the optimal variance
#   min_sd <- sqrt(min_var / (n - 1))
#   return(min_sd)
# }


