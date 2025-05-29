#' Calculate TIDES test for multiple sets of values
#'
#' Explanation to be added
#' 
#' @import dplyr
#' @import tidyr
#' @import tibble
#' @import purrr
#' @param mean numeric variable representing the reported mean.
#' @param sd numeric variable representing the reported Standard Deviation.
#' @param n numeric variable representing the reported sample size.
#' @param min numeric variable representing the variable's minimum possible/observable score.
#' @param max numeric variable representing the variable's maximum possible/observable score.
#' @param n_items number of items averaged over
#' @param digits reported to
#' @param calculate_min_sd logical variable representing whether a minimum SD should also be calculated. This should only be calculated if the varible is not only trucated (has a minimum and maximum possible/observable score) but also the variable is discrete/binned/granular: ie the response must be whole numbers (e.g., a 1-7 likert scale, where an indiviudal cannot provide a score of 1.5).
#' @returns a tibble containing the max and min SD and a summary variable `result` indicating if the tested values are consistent or not.
#' @examples
#' \dontrun{
#' # check multiple results 
#' dat <- tibble(mean = c(1, 1.2, 1.4), 
#'               sd   = c(0.5, 0.5, 0.6),
#'               n    = c(30, 30, 35),
#'               min  = 1,
#'               max  = c(7, 5, 7),
#'               n_items = 1,
#'               digits = 2,
#'               calculate_min_sd = TRUE,
#'               verbose = FALSE) 
#' 
#' tides_multiple(mean = dat$mean,
#'                sd = dat$sd,
#'                n = dat$n,
#'                min = dat$min,
#'                max = dat$max)
#' }
#' @export 
tides_multiple <- function(mean, sd, n, min, max, n_items = 1, digits, calculate_min_sd = TRUE){
  tibble(mean = mean,
         sd = sd,
         n = n,
         min = min,
         max = max,
         n_items = n_items,
         digits = digits,
         calculate_min_sd = calculate_min_sd) |>
    mutate(results = pmap(list(mean, sd, n, min, max, n_items, digits, calculate_min_sd, verbose = FALSE),
                          tides)) |>
    unnest(results)
}
