#' TIDES consistency test for a single mean–SD report
#'
#' Given a reported mean, standard deviation and sample size on a bounded scale,
#' \code{tides()} checks whether the observed SD is feasible under the known
#' minimum/maximum of that scale (and optional item‐level discretization). It
#' returns the theoretical minimum and maximum SD, the Percent Of Maximum
#' Possible (POMP) transformations of the mean and SD, and logical flags
#' indicating whether the reported values fall within the feasible range.
#'
#' If exact SD bounds cannot be computed for the specified mean, the function
#' can optionally fall back to using bounds from the closest nearby mean with
#' calculable SD bounds. This is controlled by the \code{method} argument.
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
#' @param calculate_min_sd Logical. If \code{TRUE}, computes the minimum feasible SD;
#'                       if \code{FALSE}, sets \code{min_sd = 0}. Defaults to \code{TRUE}.
#' @param verbose        Logical. If \code{TRUE}, prepends the input arguments to
#'                       the output for easy reference. Defaults to \code{TRUE}.
#' @param method         Character string. Either \code{"exact"} (default) to only
#'                       compute SD bounds for the reported mean, or \code{"approximate"}
#'                       to allow fallback to a nearby mean with defined bounds if the
#'                       requested mean has no valid SD bounds.
#' @param approximate_bounds_range Integer. Number of steps (based on \code{digits}) in
#'                       either direction to search for a nearby mean with valid SD bounds
#'                       when \code{method = "approximate"}. Defaults to 10 (i.e., ±10 steps).
#'
#' @return A data frame (or tibble) with the following columns:
#' \describe{
#'   \item{relative_location}{Observed mean as a proportion of the possible range: \((\text{mean}-\text{min})/(\text{max}-\text{min})\).}
#'   \item{relative_dispersion}{Observed SD as a proportion of the possible range: 
#'                  \((\text{sd}-\text{min\_sd})/(\text{max\_sd}-\text{min\_sd})\), or \code{NA} if undefined.}
#'   \item{min_sd}{Minimum feasible SD (or 0 if \code{calculate_min_sd = FALSE}).}
#'   \item{max_sd}{Maximum feasible SD.}
#'   \item{sd_range_calculable}{\code{TRUE} if both \code{min_sd} and \code{max_sd} are non-\code{NA}.}
#'   \item{mean_inside_range}{\code{TRUE} if \code{mean} lies between \code{min} and \code{max}.}
#'   \item{sd_inside_range}{\code{TRUE} if \code{sd} lies within the computed SD bounds (lower
#'                          bound only enforced when \code{calculate_min_sd = TRUE}).}
#'   \item{inside_ranges}{\code{TRUE} if both \code{mean_inside_range} and \code{sd_inside_range}.}
#'   \item{tides_consistent}{\code{TRUE} if \code{sd_range_calculable} and \code{inside_ranges}.}
#' }
#' If \code{verbose = TRUE}, the input arguments
#' (\code{mean}, \code{sd}, \code{n}, \code{min}, \code{max},
#' \code{n_items}, \code{digits}, \code{calculate_min_sd}, \code{method})
#' appear as leading columns in the returned data frame.
#'
#' @examples
#' \dontrun{
#' # Single case on a 1–5 scale
#' tides(mean = 3.20, sd = 0.80, n = 30, min = 1, max = 5)
#' 
#' # Results can be inside the bounds of a tides plot and still inconsistent because they are (implicitly) GRIM or GRIMMER inconsistent.
#' # See plot_tides() for an illustration.
#' tides(mean = 3.21, sd = 0.80, n = 30, min = 1, max = 5)
#' 
#' # A more liberal test against the extreme bounds of SD for nearby means is also possible, i.e., a TIDES-only test ignoring GRIM/MER inconsistencies.
#' tides(mean = 3.21, sd = 0.80, n = 30, min = 1, max = 5, method = "approximate")
#'
#' # Only compute upper SD bound, eg if measurement instrument has arbitrary granularity
#' tides(mean = 3.2, sd = 0.8, n = 30, min = 1, max = 5, calculate_min_sd = FALSE)
#' }
#' 
#' @importFrom dplyr mutate bind_cols slice_min
#' @importFrom tidyr expand_grid unnest
#' @importFrom janitor round_half_up
#' @importFrom purrr pmap 
#' 
#' @export 
tides <- function(mean, sd, n, min, max,
                  n_items = 1, 
                  digits = 2,
                  calculate_min_sd = TRUE,
                  verbose = TRUE,
                  method = c("exact", "approximate"),
                  approximate_bounds_range = 10) {
  
  # check inputs
  method <- match.arg(method)
  
  # try exact bounds first
  bounds <- sd_bounds(mean, n, min, max, n_items, digits)
  min_sd <- bounds$min_sd
  max_sd <- bounds$max_sd

  # optionally fallback to liberal bounds if strict ones are NA
  if (method == "approximate" & is.na(max_sd)) {
    step <- 10^(-digits) 
    range <- approximate_bounds_range * step
    mean_grid <- seq(mean - range, mean + range, by = step)
    
    # search nearby means for SD bounds
    grid <- expand_grid(
      mean = mean_grid,
      n = n,
      n_items = n_items,
      min = min,
      max = max,
      precision = digits,
      calculate_min_sd = calculate_min_sd
    ) |>
      mutate(sd_bounds = purrr::pmap(list(mean = mean, n = n, min = min, max = max,
                                          n_items = n_items, digits = precision,
                                          calculate_min_sd = calculate_min_sd),
                                     sd_bounds)) |>
      tidyr::unnest(sd_bounds)
    
    # fill nearest and more extreme bound to the missing ones
    filled <- grid |>
      filter(mean >= min, mean <= max) |>
      approximate_sd_bounds()
    
    # return not the mean tested but the one that has a matching bound
    bounds_fallback <- filled |>
      slice_min(abs(mean - !!mean), n = 1)
    
    min_sd <- bounds_fallback$min_sd
    max_sd <- bounds_fallback$max_sd
    
    if(is.na(max_sd)) {
      stop("No approximate bounds found for nearby means. Try increasing approximate_bounds_range to widen the search for means with defined SD bounds.")
    }
  }
  
  # always override lower bound if calculate_min_sd is FALSE
  if (!calculate_min_sd && !is.na(max_sd)) {
    min_sd <- 0
  }
  
  # proportion of maximum possible transformation
  pomp_mean <- (mean - min) / (max - min)
  
  pomp_sd <- if(!is.na(min_sd) && !is.na(max_sd)) {
    (sd - min_sd) / (max_sd - min_sd)
  } else {
    NA_real_
  }
  if(is.infinite(pomp_sd) || is.nan(pomp_sd)) {
    pomp_sd <- NA_real_
  }
  
  # combine results
  df <- data.frame(
    relative_location = janitor::round_half_up(pomp_mean, digits+2),
    relative_dispersion   = janitor::round_half_up(pomp_sd,   digits+2),
    min_sd    = janitor::round_half_up(min_sd,    digits),
    max_sd    = janitor::round_half_up(max_sd,    digits)
  ) |>
    mutate(sd_range_calculable = !is.na(min_sd) & !is.na(max_sd),
           mean_inside_range = mean >= min & mean <= max,
           sd_inside_range = case_when(
             calculate_min_sd & !is.na(min_sd) & !is.na(max_sd) ~ sd >= min_sd & sd <= max_sd,
             !calculate_min_sd & !is.na(max_sd) ~ sd <= max_sd,
             TRUE ~ FALSE
           ),
           inside_ranges = mean_inside_range & sd_inside_range,
           tides_consistent = sd_range_calculable & inside_ranges)
  
  # add metadata if verbose
  if (verbose) {
    meta <- data.frame(
      mean = mean, 
      sd = sd, 
      n = n, 
      min = min, 
      max = max, 
      n_items = n_items,
      digits = if (is.null(digits)) {
        NA_integer_
      } else { 
        digits
      },
      calculate_min_sd = calculate_min_sd,
      method = method
    )
    df <- dplyr::bind_cols(meta, df)
  }
  
  return(df)
}



