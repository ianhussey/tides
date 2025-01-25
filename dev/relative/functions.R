
library(tidyverse)
library(purrr)
library(scales)
library(matrixStats) # ror efficient row-wise statistics
library(TIDES)

#' calculate M and SD for a random subset of N participants
summary_stats <- function(data, sample_n){
  data |>
    sample_n(size = sample_n) |>
    summarize(n = n(),
              mean = mean(score),
              sd = sd(score))
}


#' bootstrap M and SD for random subsets of N participants
bootstrap_summary_stats <- function(data, sample_n, iterations, n_digits_reported = 1) {
  map_dfr(1:iterations, ~ summary_stats(data, sample_n)) |>
    mutate(mean = janitor::round_half_up(mean, n_digits_reported),
           sd = janitor::round_half_up(sd, n_digits_reported)) |>
    dplyr::count(n, mean, sd, name = "frequency")
}


#' generate all possible means and SDs for a given N, min and max score, and number of reported digits
#' runs faster and produces more sensible looking SDs
#' note that digits should always be at least n_reported_digits + 1 or else you need to consider what rounding method they used!
#' @param n number of participants * number of items averaged over (if mean scores for a multi-item scale)
#' @param min_score minimum possible score
#' @param max_score maximum possible score
#' @param n_digits_reported number of digits to which the reported means and SDs were rounded to
values_of_possible_means_and_sds <- function(n, min_score, max_score, n_digits_reported = 1) {
  
  if(n >= n_digits_reported * 100){
    stop(paste0("When n >= ", n_digits_reported, " * 100, there are no granularity constraints on the mean or SD, only limit constraints. Use other TIDES functions to find the limits mathematically instead."))
  }
  
  # generate all possible scores
  scores <- min_score:max_score
  
  # function to generate combinations with replacement
  combinations_with_replacement <- function(x, m) {
    if (m == 1) {
      matrix(x, ncol = 1)
    } else {
      do.call(rbind, lapply(seq_along(x), function(i) {
        cbind(x[i], combinations_with_replacement(x[i:length(x)], m - 1))
      }))
    }
  }
  
  # generate all unique combinations with replacement for n participants
  all_combinations <- combinations_with_replacement(scores, n)
  
  # ensure unique combinations by sorting rows and removing duplicates
  all_combinations <- unique(t(apply(all_combinations, 1, sort)))
  
  # calculate Mean and SD for each unique combination
  means <- rowMeans(all_combinations)
  sds <- rowSds(all_combinations)
  
  results <- 
    tibble(n = n,
           mean = means, 
           sd = sds) |>
    mutate(mean = round(mean, n_digits_reported+1),
           sd = round(sd, n_digits_reported+1)) |>
    distinct(n, mean, sd)
  
  return(results)
}

values_plot <- function(n, min_score, max_score, n_digits_reported = 1){
  
  res <- values_of_possible_means_and_sds(n, min_score, max_score, n_digits_reported)
  
  ggplot(res, aes(mean, sd)) +
    geom_point(shape = "square", size = 0.5) +
    scale_x_continuous(limits = c(min_score, max_score),
                       breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = c(0, max(res$sd)),
                       breaks = scales::breaks_pretty()) +
    theme_linedraw() +
    xlab("Mean") +
    ylab("SD")
}

sprite_plot <- values_plot


limits_of_possible_means_and_sds <- function(n, min_score, max_score, n_digits_reported = 1, calculate_min_sd = TRUE){
  results <- 
    expand_grid(min = min_score,
                max = max_score,
                mean = seq(from = min_score, to = max_score, by = 1/10^n_digits_reported),
                n = n) |>
    mutate(sd_max = pmap(list(mean, n, min, max), max_sd)) |>
    unnest(sd_max) |>
    select(n, min, max, mean, sd_max)
  
  if(calculate_min_sd){
    results <- results |>
      mutate(sd_min = pmap(list(mean, n), min_sd)) |>
      unnest(sd_min) |>
      select(n, min, max, mean, sd_min, sd_max)
  }
  
  return(results)
}

# range normalize M and SD
range_normalize <- function(data, n, min_score, max_score){
  
  max_sd <- max_sd(mean = (max_score - min_score)/2,
                   n = n,
                   min = min_score,
                   max = max_score)

  data |>
    mutate(mean = (mean - min_score) / (max_score - min_score),
           sd = sd/max_sd)
}

limits_plot <- function(n, min_score, max_score, n_digits_reported = 1){
  
  res <- limits_of_possible_means_and_sds(n, min_score, max_score, n_digits_reported)
  
  ggplot(res) +
    geom_line(aes(mean, sd_min)) +
    geom_line(aes(mean, sd_max)) +
    scale_x_continuous(limits = c(min_score, max_score),
                       breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = c(0, max(res$sd_max)),
                       breaks = scales::breaks_pretty()) +
    theme_linedraw() +
    xlab("Mean") +
    ylab("SD")
}


standardized_limits_plot <- function(n, min_score, max_score, n_digits_reported = 1){
  
  res <- limits_of_possible_means_and_sds(n, min_score, max_score, n_digits_reported) |>
    mutate(mean = (mean - min_score) / (max_score - min_score),
           sd_min = sd_min/max(sd_max),
           sd_max = sd_max/max(sd_max))
  
  ggplot(res) +
    geom_line(aes(mean, sd_min)) +
    geom_line(aes(mean, sd_max)) +
    scale_x_continuous(limits = c(0, 1),
                       breaks = scales::breaks_pretty()) +
    scale_y_continuous(limits = c(0, 1),
                       breaks = scales::breaks_pretty()) +
    theme_linedraw() +
    xlab("POMP mean") +
    ylab("POMP SD")
}





