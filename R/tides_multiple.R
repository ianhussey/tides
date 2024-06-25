#' Calculate TIDES test for multiple sets of values
#'
#' Explanation to be added
#' 
#' @param mean numeric variable representing the reported mean.
#' @param sd numeric variable representing the reported Standard Deviation.
#' @param n numeric variable representing the reported sample size.
#' @param min numeric variable representing the variable's minimum possible/observable score.
#' @param max numeric variable representing the variable's maximum possible/observable score.
#' @param calculate_min_sd logical variable representing whether a minimum SD should also be calculated. This should only be calculated if the varible is not only trucated (has a minimum and maximum possible/observable score) but also the variable is discrete/binned/granular: ie the response must be whole numbers (e.g., a 1-7 likert scale, where an indiviudal cannot provide a score of 1.5).
tides_multiple <- function(mean, sd, n, min, max, calculate_min_sd = FALSE){
  tibble(mean = mean,
         sd = sd,
         n = n,
         min = min,
         max = max) |>
    mutate(results = pmap(list(mean, sd, n, min, max, calculate_min_sd, verbose = FALSE), tides_single)) |>
    unnest(results)
}
