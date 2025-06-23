#' Generate all GRIM+GRIMMER+TIDES–consistent means and SDs for an umbrella plot
#'
#' Given a sample size, scale bounds, response‐item count, and decimal precision,
#' this function builds the full grid of reported means and standard deviations
#' that simultaneously satisfy:
#'   1. GRIM (integer‐mean) constraints,
#'   2. GRIMMER (decimal‐SD) constraints, and
#'   3. TIDES (bounds‐test) constraints.
#'
#' @param n         Integer. Sample size.
#' @param min       Numeric. Minimum possible (or observed) score.
#' @param max       Numeric. Maximum possible (or observed) score.
#' @param n_items   Integer(1). Number of discrete items per participant
#'                  (e.g. Likert‐scale items). Defaults to 1.
#' @param precision Integer. Number of decimal places in reported means/SDs;
#'                  used to define the mean‐grid and to restore trailing zeros.
#'                  Defaults to 2.
#'
#' @return A tibble with columns:
#'   \describe{
#'     \item{mean}{A feasible reported mean (numeric).}
#'     \item{sd}{A feasible reported standard deviation (numeric).}
#'   }
#'   Each row corresponds to one (mean, SD) pair that passes GRIM, GRIMMER, and TIDES.
#'
#' @examples
#' \dontrun{
#' # All possible means and SDs for N=14, scale 1–7, two decimal places:
#' df <- umbrella(n = 14, min = 1, max = 7, n_items = 1, precision = 2)
#' head(df)
#' }
#'
#' @importFrom janitor round_half_up
#' @importFrom purrr map map2
#' @importFrom tibble tibble
#' @importFrom tidyr unnest
#' @importFrom scrutiny restore_zeros
#' @importFrom dplyr filter mutate select
#' @export
umbrella <- function(n, min, max, n_items = 1, precision = 2){
  
  res <- 
    # 1. find all GRIM consistent means and the min/max of their SD
    
    # define all possible values of mean from min to max in increments of precision
    tibble(mean = seq(from = min, to = max, by = 10^-precision)) |> 
    # generate min and max SD for each mean, which may ba NA. This generates only TIDES consistent values within the bounds.
    mutate(sd_bounds = map(mean, ~ sd_bounds(mean    = .x,
                                             n       = n,
                                             min     = min,
                                             max     = max,
                                             n_items = n_items,
                                             digits  = precision))) |>
    unnest(sd_bounds) |>
    # drop means for which no feasible SD‐range exists, so that your remaining means are now GRIM consistent with at least the min and max also being GRIMMER consistent 
    filter(!is.na(min_sd), !is.na(max_sd)) |>
    
    # 2. find all GRIMMER consistent SDs
    
    # for each remaining mean, generate all SDs between the min and max bound in increments of precision
    mutate(sd = map2(min_sd, max_sd, ~ seq(.x, .y, by = 0.01))) |>
    unnest(sd) |>
    select(mean, sd) |>
    # test which of these SDs are GRIMMER consistent
    ## create variables needed for GRIMMER testing
    mutate(n         = n,
           precision = digits,
           n_items   = n_items,
           min       = min,
           max       = max,
           # define one rounding method to not inflate baseline pass rate
           rounding = "up") |>
    ## convert M and SD to character and then restore trailing zero, as required for GRIM/MER
    mutate(mean_char = as.character(mean),
           mean_char = scrutiny::restore_zeros(mean_char, width = precision),
           sd_char = as.character(sd),
           sd_char = scrutiny::restore_zeros(sd_char, width = precision)) |>
    ## apply GRIMMER to the reduced grid
    mutate(grimmer = pmap(list(x        = mean_char,
                               sd       = sd_char,
                               n        = n,
                               items    = n_items,
                               rounding = rounding),
                          grimmer)) |>
    unnest(grimmer) |>
    # drop GRIMMER inconsistent values, so that only GRIM+GRIMMER+TIDES consistent values remain
    filter(grimmer == TRUE) |>
    select(-mean_char, -sd_char, -rounding, -grimmer)
  
  return(res)
}



