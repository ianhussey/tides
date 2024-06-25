#' Calculate TIDES test for a single set of values
#'
#' Explanation to be added
#' 
#' @param mean numeric variable representing the reported mean.
#' @param sd numeric variable representing the reported Standard Deviation.
#' @param n numeric variable representing the reported sample size.
#' @param min numeric variable representing the variable's minimum possible/observable score.
#' @param max numeric variable representing the variable's maximum possible/observable score.
#' @param calculate_min_sd logical variable representing whether a minimum SD should also be calculated. This should only be calculated if the varible is not only trucated (has a minimum and maximum possible/observable score) but also the variable is discrete/binned/granular: ie the response must be whole numbers (e.g., a 1-7 likert scale, where an indiviudal cannot provide a score of 1.5).
#' @param verbose logical variable representing whether the output should also contain the input values.
#' @returns a tibble containing the max and min SD and a summary variable `result` indicating if the tested values are consistent or not.
#' @export 
tides_single <- function(mean, sd, n, min, max, calculate_min_sd = FALSE, verbose = TRUE){
  
  # calculate the maximum standard deviation
  max_sd <- max_sd(mean, sd, n, min, max)
  
  # calculate the minimum standard deviation using the provided function
  if (calculate_min_sd) {
    min_sd <- min_sd(mean, n)
  } else {
    min_sd <- NA
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
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | (sd < min_sd | is.na(min_sd)) ~ "Inconsistent",
                                     sd <= max_sd & (sd >= min_sd | is.na(min_sd)) ~ "Consistent"))
  } else if (!verbose){
    res <- tibble(standardized_mean = standardized_mean,
                  standardized_sd = standardized_sd,
                  max_standardized_sd = max_standardized_sd,
                  min_sd = min_sd,
                  max_sd = max_sd,
                  result = case_when(sd > max_sd | (sd < min_sd | is.na(min_sd)) ~ "Inconsistent",
                                     sd <= max_sd & (sd >= min_sd | is.na(min_sd)) ~ "Consistent"))
  }
  
  return(res)
}
