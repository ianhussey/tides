#' Calculate TIDES test for a single set of values
#'
#' Explanation to be added. TODO: consider use of round inside the function and in the result returned.
#' 
#' @param mean numeric variable representing the reported mean.
#' @param sd numeric variable representing the reported Standard Deviation.
#' @param n numeric variable representing the reported sample size.
#' @param min numeric variable representing the variable's minimum possible/observable score.
#' @param max numeric variable representing the variable's maximum possible/observable score.
#' @param n_items numeric variable representing how man items were averaged over at the participant level when creating the mean. I.e., when a single item Likert scale or a sum score of a multi-item Likert scale, no prior participant level averaging occured, only averaging across participants, so items = 1. If a multi item scale and mean score across items was used, enter the number of items here.
#' @param calculate_min_sd logical variable representing whether a minimum SD should also be calculated. This should only be calculated if the variable is not only truncated (has a minimum and maximum possible/observable score) but also the variable is discrete/binned/granular: ie the response must be whole numbers (e.g., a 1-7 likert scale, where an indiviudal cannot provide a score of 1.5).
#' @param verbose logical variable representing whether the output should also contain the input values.
#' @returns a tibble containing the max and min SD and a summary variable `tides` indicating if the tested values are consistent or not. ADD NOTES ON OTHER COLUMNS RETURNED.
#' @export 
tides_single <- function(mean, sd, n, min, max, n_items = 1, digits = NULL,
                         calculate_min_sd = TRUE, verbose = TRUE){
  
  if (is.null(digits)) {
    digits <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }
  
  result <- c(-Inf, Inf)
  
  min_alpha <- min
  max_alpha <- floor(mean*n_items)/n_items
  max_beta <- min(max(max, min + 1, max_alpha + 1), max)
  min_beta <- min(max_alpha + 1/n_items, max)
  total <- round(mean * n * n_items)/n_items
  
  poss_values <- max
  for (i in seq_len(n_items)) {
    poss_values <- c(poss_values, min:(max-1) + (1 / n_items) * (i - 1))
  }
  poss_values <- sort(poss_values)
  
  for (abm in list(c(max_alpha, min_beta, 1), c(min_alpha, max_beta, 2))) {
    
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]
    
    # Adjust a and b to be within min and max
    a <- min(max(a, min), max)
    b <- min(max(b, min), max)
    
    if (a == b) {
      vec <- rep(a, n)
    } else {
      k <- round((total - (n * b)) / (a - b))
      k <- min(max(k, 1), n - 1)
      vec <- c(rep(a, k), rep(b, n - k))
      diff <- sum(vec) - total
      
      if ((diff < 0)) {
        vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n - k))
      } else if ((diff > 0)) {
        vec <- c(rep(a, k), b - diff, rep(b, n - k - 1))
      }
    }
    
    # Check if the calculated mean and values match expected conditions
    if (round(mean(vec), digits) == round(mean, digits) & all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
      result[m] <- round(sd(vec), digits)
    }
    
  }
  
  # replace Inf or -Inf with NA
  result[is.infinite(result)] <- NA
  
  min_sd <- result[1]
  max_sd <- result[2]
  
  
  # Percent Of Maximum Possible (POMP) mean and SD
  # calculated prior to rounding
  pomp_mean <- (mean - min)/(max - min)
  pomp_sd <- ifelse(!is.na(min_sd) & !is.na(max_sd), (sd - min_sd)/(max_sd - min_sd), NA)
  # when SD is zero, this divides by zero and returns Inf. Replace these with 0 as a dummy value so that they are plotted rather than ignored.
  if (is.infinite(pomp_sd) | is.nan(pomp_sd)) { pomp_sd <- 0 }
  
  # results
  res <- data.frame(pomp_mean = janitor::round_half_up(pomp_mean, 4),
                    pomp_sd = janitor::round_half_up(pomp_sd, 4),
                    min_sd = janitor::round_half_up(min_sd, digits), 
                    max_sd = janitor::round_half_up(max_sd, digits)) |>
    mutate(min_sd = case_when(calculate_min_sd ~ min_sd,
                              !calculate_min_sd ~ 0),
           sd_range_calculable = !is.na(min_sd) & !is.na(max_sd),
           mean_inside_range = mean >= min & mean <= max,
           sd_inside_range = case_when(calculate_min_sd & sd_range_calculable ~ sd >= min_sd & sd <= max_sd,
                                       !calculate_min_sd & sd_range_calculable ~ sd <= max_sd,
                                       TRUE ~ FALSE),
           inside_ranges = mean_inside_range & sd_inside_range,
           tides_consistent = sd_range_calculable & inside_ranges)
  
  if(verbose){
    res2 <- data.frame(mean = mean,
                       sd = sd,
                       n = n,
                       min = min,
                       max = max,
                       n_items = n_items, 
                       digits = digits,
                       calculate_min_sd = calculate_min_sd)
    
    res <- bind_cols(res2,
                     res)
  }
  
  return(res)
}



