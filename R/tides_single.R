#' Calculate TIDES test for a single set of values
#'
#' Explanation to be added
#'
#' @param mean Numeric. Reported mean.
#' @param sd Numeric. Reported standard deviation.
#' @param n Numeric. Reported sample size.
#' @param min Numeric. Minimum possible/observable score.
#' @param max Numeric. Maximum possible/observable score.
#' @param calculate_min_sd Logical. Should a minimum SD also be calculated? Only
#'   set if to `TRUE` if the variable is not only truncated (it has a minimum
#'   and maximum possible/observable score) but also discrete/binned/granular:
#'   the response must be whole numbers (e.g., a 1-7 Likert scale, where an
#'   individual cannot provide a score of 1.5). Default is `FALSE`.
#' @param verbose Logical. Should the output also contain the input values?
#'   Default is `TRUE`.
#'
#' @returns A tibble containing the max and min SD and a summary variable
#'   `consistency` indicating if the tested values are consistent or not.
#'
#' @export
#'
#' @examples
#' tides_single(mean = 1.1, sd = 0.5, n = 12, min = 1, max = 7)

tides_single <- function(mean,
                         sd,
                         n,
                         min,
                         max,
                         calculate_min_sd = FALSE,
                         verbose = TRUE) {

  # calculate the maximum standard deviation
  max_sd <- max_sd(mean, n, min, max)

  # calculate the minimum standard deviation using the provided function
  min_sd <- if (calculate_min_sd) {
    min_sd(mean, n)
  } else {
    NULL
  }

  # Standardize the mean and SD based on the maximum possible range.
  standardized_mean <- (mean - min) / (max - min)
  max_standardized_sd <- ( sqrt( (standardized_mean) * (1 - (standardized_mean)) ) ) * ( sqrt(n / (n - 1)) )
  standardized_sd <- sd / max_sd

  # Check whether the inputs are mutually consistent
  consistency <- !is.null(min_sd) && sd >= min_sd && sd <= max_sd

  # If output should be verbose (the default), prepare a named vector of inputs:
  inputs <- if (verbose) {
    c(mean = mean, sd = sd, n = n, min = min, max = max)
  } else {
    NULL
  }

  # Collect results in a tibble. Splicing the `inputs` object has no effect if
  # it's `NULL`, so no extra columns are created in that case.
  tibble(
    !!!inputs,
    consistency,
    standardized_mean,
    standardized_sd,
    max_standardized_sd,
    min_sd,
    max_sd
  )

}
