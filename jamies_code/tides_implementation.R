tides_test <- function(sample_size, scale_lower_bound, 
                       scale_upper_bound, score) {
  
  standardised_mean <- (score - scale_lower_bound) / (scale_upper_bound - scale_lower_bound)
  
  max_standardised_sd <- ( sqrt( ((standardised_mean) * (1 - (standardised_mean))) ) ) * ( sqrt(sample_size / (sample_size - 1)) )
  
  max_unstandardised_sd <- max_standardised_sd * (scale_upper_bound - scale_lower_bound)
  
  return(max_unstandardised_sd)
  
}


tides_test(sample_size = 350, 
           scale_lower_bound = 1, 
           scale_upper_bound = 7, 
           score = 0)
