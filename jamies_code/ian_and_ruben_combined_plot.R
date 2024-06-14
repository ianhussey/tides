tides_test <- function(sample_size, scale_lower_bound, 
                       scale_upper_bound, score) {
  
  standardised_mean <- (score - scale_lower_bound) / (scale_upper_bound - scale_lower_bound)
  
  max_standardised_sd <- ( sqrt( ((standardised_mean) * (1 - (standardised_mean))) ) ) * ( sqrt(sample_size / (sample_size - 1)) )
  
  max_unstandardised_sd <- max_standardised_sd * (scale_upper_bound - scale_lower_bound)
  
  return(max_unstandardised_sd)
  
}

calculate_min_sd <- function(N, mean) {
  # Determine the closest integer values around the mean
  lower_value <- floor(mean)
  upper_value <- ceiling(mean)
  
  # Calculate frequencies required to achieve the mean
  total_sum <- N * mean
  lower_count <- N * (upper_value - mean)
  upper_count <- N * (mean - lower_value)
  
  # If the calculated counts are not integers, round them appropriately
  lower_count <- round(lower_count)
  upper_count <- round(upper_count)
  
  # Ensure the total count equals N
  if (lower_count + upper_count != N) {
    lower_count <- N - upper_count
  }
  
  # Form the combination of values
  values <- c(rep(lower_value, lower_count), rep(upper_value, upper_count))
  
  # Calculate the mean and standard deviation
  actual_mean <- mean(values)
  sd_value <- sd(values)
  
  return(list(mean = actual_mean, sd = sd_value))
}

N <- c(300)
scale_lower_bound <- 1
scale_upper_bound <- 6
granularity <- (scale_upper_bound / scale_upper_bound) / 10

test_data <- tibble(
  x_value = rep(seq(scale_lower_bound, scale_upper_bound, granularity), length(N)),
  N = rep(N, each = length(seq(scale_lower_bound, scale_upper_bound, granularity))),
) |>
  rowwise() |>
  mutate(
    actual_mean = calculate_min_sd(N, mean = x_value)$mean,
    minimum_sd = calculate_min_sd(N, mean = x_value)$sd,
    maximum_sd = tides_test(N, scale_lower_bound, scale_upper_bound, score = x_value)
)


test_mean <- 6
test_sd <- 1.5

test_data |>
  ggplot() +
  geom_line(
    aes(actual_mean, minimum_sd)
    ) +
  geom_line(
    aes(actual_mean, maximum_sd)
    ) +
  labs(
    y = "Possible SDs",
    x = "Actual mean"
  ) +
  facet_wrap(~N) +
  geom_point(
    aes(x = test_mean,
        y = test_sd)
  )








