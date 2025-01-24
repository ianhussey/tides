.sd_limits <- function(n_obs, mean, min_val, max_val, sd_prec = NULL, n_items = 1) {
  
  if (is.null(sd_prec)) {
    sd_prec <- max(nchar(sub("^[0-9]*", "", mean)) - 1, 0)
  }
  
  result <- c(-Inf, Inf)
  
  aMax <- min_val                                # "aMax" means "value of a to produce the max SD"
  aMin <- floor(mean*n_items)/n_items
  bMax <- max(max_val, min_val + 1, aMin + 1)   # sanity check (just max_val would normally be ok)
  bMin <- aMin + 1/n_items
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
    
    
    k <- round((total - (n_obs * b)) / (a - b))
    k <- min(max(k, 1), n_obs - 1)               # ensure there is at least one of each of two numbers
    vec <- c(rep(a, k), rep(b, n_obs - k))
    diff <- sum(vec) - total
    
    if ((diff < 0)) {
      vec <- c(rep(a, k - 1), a + abs(diff), rep(b, n_obs - k))
    }
    else if ((diff > 0)) {
      vec <- c(rep(a, k), b - diff, rep(b, n_obs - k - 1))
    }
    
    # # Debugging: Print relevant information to diagnose the issue
    # cat("Iteration for a =", a, "b =", b, "\n")
    # cat("Generated vec:", vec, "\n")
    # cat("Mean of vec:", mean(vec), "Expected mean:", mean, "\n")
    # cat("SD of vec:", sd(vec), "\n\n")
    
    if (round(mean(vec), sd_prec) != round(mean, sd_prec) | !all(floor(vec*10e9) %in% floor(poss_values*10e9))) {
      stop("Error in calculating range of possible standard deviations")
    }
    
    result[m] <- round(sd(vec), sd_prec)
  }
  
  return(result)
}