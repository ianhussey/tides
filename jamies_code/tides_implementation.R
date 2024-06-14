tides_test <- function(sample_size, scale_lower_bound, 
                       scale_upper_bound, score) {
  
  standardised_mean <- (score - scale_lower_bound) / (scale_upper_bound - scale_lower_bound)
  
  max_standardised_sd <- ( sqrt( ((standardised_mean) * (1 - (standardised_mean))) ) ) * ( sqrt(sample_size / (sample_size - 1)) )
  
  max_unstandardised_sd <- max_standardised_sd * (scale_upper_bound - scale_lower_bound)
  
  return(max_unstandardised_sd)
  
}


tides_test(sample_size = 350, 
           scale_lower_bound = 1, 
           scale_upper_bound = 6, 
           score = 2.5)

# quadratic approximation 
fit_quadratic <- function(scale_lower_bound, scale_upper_bound) {
  
  data <- tibble(
    x = c(scale_lower_bound, (scale_lower_bound + scale_upper_bound) / 2, scale_upper_bound),
    y = c(0, (scale_upper_bound - scale_lower_bound) / 2, 0)
  )
  
  # Fit a quadratic model
  model <- lm(y ~ poly(x, 2, raw = TRUE), data = data)
  
  return(model)
}



# check if the y value exceeds the quadratic curve
check_y_value <- function(model, x, y) {
  
  # Predict sd using the quadratic model
  y_pred <- predict(model, newdata = tibble(x = x))
  
  # Check if the given y value exceeds the predicted y value
  exceeds <- y > y_pred
  
  return(exceeds)
}


# Fit model
model <- fit_quadratic(scale_lower_bound = 1,
                       scale_upper_bound = 6)

# Coordinates to check
x <- 2.5
y <- 2.5

preds <- predict(model, tibble(x = seq(1, 6, .005)))

plot(preds)

# Check if the y value exceeds the quadratic curve
exceeds <- check_y_value(model, x, y)
print(exceeds)
