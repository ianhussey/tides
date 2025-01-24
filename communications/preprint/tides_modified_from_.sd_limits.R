tides_modified_from_.sd_limits <- function(n_obs, mean, sd, min_val, max_val, sd_prec = NULL, n_items = 1) {
  
  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }
  
  result <- c(-Inf, Inf)
  
  aMax <- min_val
  aMin <- floor(mean*n_items)/n_items
  bMax <- min(max(max_val, min_val + 1, aMin + 1), max_val)   # Adjusted here
  bMin <- min(aMin + 1/n_items, max_val)                      # Adjusted here
  total <- round(mean * n_obs * n_items)/n_items
  
  poss_values <- max_val
  for (i in seq_len(n_items)) {
    poss_values <- c(poss_values, min_val:(max_val-1) + (1 / n_items) * (i - 1))
  }
  poss_values <- sort(poss_values)
  
  for (abm in list(c(aMin, bMin, 1), c(aMax, bMax, 2))) {
    
    a <- abm[1]
    b <- abm[2]
    m <- abm[3]
    
    # Adjust a and b to be within min_val and max_val
    a <- min(max(a, min_val), max_val)
    b <- min(max(b, min_val), max_val)
    
    if (a == b) {
      vec <- rep(a, n_obs)
    } else {
      k <- round((total - (n_obs * b)) / (a - b))
      k <- min(max(k, 1), n_obs - 1)
      vec <- c(rep(a, k), rep(b, n_obs - k))
      diff <- sum(vec) - total
      
      if ((diff < 0)) {
        vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n_obs - k))
      } else if ((diff > 0)) {
        vec <- c(rep(a, k), b - diff, rep(b, n_obs - k - 1))
      }
    }
    
    # instead of throwing errors here, return NA
    # if (round(mean(vec), sd_prec) != round(mean, sd_prec) | !all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
    #   stop("Error in calculating range of possible standard deviations")
    # }
    # result[m] <- round(sd(vec), sd_prec)
    
    # Check if the calculated mean and values match expected conditions
    if (round(mean(vec), sd_prec) == round(mean, sd_prec) & all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
      result[m] <- round(sd(vec), sd_prec)
    }
    
  }
  
  # Replace Inf or -Inf with NA
  result[is.infinite(result)] <- NA
  
  # returns df instead of vector
  res <- 
    data.frame(sd_min = result[1],
               sd_max = result[2]) |>
    mutate(tides = case_when(sd < sd_min ~ FALSE,
                             is.na(sd_min) ~ FALSE,
                             sd > sd_max ~ FALSE,
                             is.na(sd_max) ~ FALSE,
                             TRUE ~ TRUE))
  
  return(res)
}