#' TIDES consistency test for a single mean–SD report
#'
#' Given a reported mean, standard deviation and sample size on a bounded scale,
#' \code{tides()} checks whether the observed SD is feasible under the known
#' minimum/maximum of that scale (and optional item‐level discretization). It
#' returns the theoretical minimum and maximum SD, the Percent Of Maximum
#' Possible (POMP) transformations of the mean and SD, and logical flags
#' indicating whether the reported values fall within the feasible range.
#'
#' @param mean           Numeric. Reported sample mean.
#' @param sd             Numeric. Reported sample standard deviation.
#' @param n              Integer. Sample size.
#' @param min            Numeric. Minimum possible (or observed) score on the scale.
#' @param max            Numeric. Maximum possible (or observed) score on the scale.
#' @param n_items        Integer ≥ 1. Number of discrete “items” averaged at the
#'                       participant level (e.g.\ a 5-item Likert mean → 5). Defaults to 1.
#' @param digits         Integer or \code{NULL}. Decimal places to use when
#'                       comparing means and rounding SD. If \code{NULL}, inferred
#'                       from the precision of \code{mean}.
#' @param calculate_min_sd
#'                       Logical. If \code{TRUE}, computes the minimum feasible SD;
#'                       if \code{FALSE}, sets \code{min_sd = 0}. Defaults to \code{TRUE}.
#' @param verbose        Logical. If \code{TRUE}, prepends the input arguments to
#'                       the output for easy reference. Defaults to \code{TRUE}.
#'
#' @return A data frame (or tibble) with these columns:
#' \describe{
#'   \item{pomp_mean}{Mean transformed to a 0–1 scale: \((\text{mean}-\text{min})/(\text{max}-\text{min})\).}
#'   \item{pomp_sd}{Observed SD as a proportion of the achievable range: 
#'                  \((\text{sd}-\text{min\_sd})/(\text{max\_sd}-\text{min\_sd})\), or 0 if undefined.}
#'   \item{min_sd}{Minimum feasible SD (or 0 if \code{calculate_min_sd = FALSE}).}
#'   \item{max_sd}{Maximum feasible SD.}
#'   \item{sd_range_calculable}{\code{TRUE} if both \code{min_sd} and \code{max_sd} are non-\code{NA}.}
#'   \item{mean_inside_range}{\code{TRUE} if \code{mean} lies between \code{min} and \code{max}.}
#'   \item{sd_inside_range}{\code{TRUE} if \code{sd} lies within the computed SD bounds (lower
#'                          bound only enforced when \code{calculate_min_sd = TRUE}).}
#'   \item{inside_ranges}{\code{TRUE} if both \code{mean_inside_range} and \code{sd_inside_range}.}
#'   \item{tides_consistent}{\code{TRUE} if \code{sd_range_calculable} and \code{inside_ranges}.}
#' }
#' If \code{verbose = TRUE}, the input arguments (\code{mean}, \code{sd}, \code{n}, \code{min}, 
#' \code{max}, \code{n_items}, \code{digits}, \code{calculate_min_sd}) appear as leading columns.
#'
#' @examples
#' \dontrun{
#' # Single case on a 1–5 scale
#' tides(mean = 3.2, sd = 0.8, n = 30, min = 1, max = 5)
#'
#' # Only compute upper SD bound
#' tides(3.2, 0.8, 30, 1, 5, calculate_min_sd = FALSE)
#'
#' # Use in a purrr workflow for multiple reports
#' library(dplyr); library(purrr); library(tidyr)
#' dat <- tibble(
#'   mean = c(2.5, 4.0, 3.1),
#'   sd   = c(0.6, 1.2, 0.9),
#'   n    = c(50, 75, 60),
#'   min  = 1, max = 7
#' )
#' dat %>%
#'   mutate(results = pmap(list(mean, sd, n, min, max), tides)) %>%
#'   unnest(results)
#' }
#'
#' @importFrom dplyr mutate bind_cols
#' @importFrom janitor round_half_up
#' 
#' @export 
tides <- function(mean, sd, n, min, max,
                  n_items = 1, digits = NULL,
                  calculate_min_sd = TRUE,
                  verbose = TRUE) {
  
  # get the bounds
  bounds <- sd_bounds(mean, n, min, max, n_items, digits)
  min_sd <- bounds[1]; max_sd <- bounds[2]
  
  # POMP transformations
  pomp_mean <- (mean - min) / (max - min)
  pomp_sd   <- if (!is.na(min_sd) && !is.na(max_sd)) {
    (sd - min_sd) / (max_sd - min_sd)
  } else {
    NA_real_
  }
  # avoid Inf/NaN
  if (is.infinite(pomp_sd) || is.nan(pomp_sd)) pomp_sd <- 0
  
  # assemble result
  df <- data.frame(
    pomp_mean = janitor::round_half_up(pomp_mean, 4),
    pomp_sd   = janitor::round_half_up(pomp_sd,   4),
    min_sd    = janitor::round_half_up(min_sd,    digits),
    max_sd    = janitor::round_half_up(max_sd,    digits)
  )
  
  df <- dplyr::mutate(df,
                      min_sd            = ifelse(calculate_min_sd, min_sd, 0),
                      sd_range_calculable = !is.na(min_sd) & !is.na(max_sd),
                      mean_inside_range = mean >= min & mean <= max,
                      sd_inside_range   = case_when(
                        calculate_min_sd & sd_range_calculable ~ sd >= min_sd & sd <= max_sd,
                        !calculate_min_sd & sd_range_calculable ~ sd <= max_sd,
                        TRUE                                    ~ FALSE
                      ),
                      inside_ranges     = mean_inside_range & sd_inside_range,
                      tides_consistent  = sd_range_calculable & inside_ranges
  )
  
  if (verbose) {
    meta <- data.frame(
      mean, sd, n, min, max, n_items,
      digits = if (is.null(digits)) NA_integer_ else digits,
      calculate_min_sd
    )
    df <- dplyr::bind_cols(meta, df)
  }
  
  return(df)
}



