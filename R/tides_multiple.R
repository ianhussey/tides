
tides_multiple <- function(mean, sd, n, min, max){
  require(purrr)
  require(dplyr)
  
  data.frame(mean = mean,
             sd = sd,
             n = n,
             min = min,
             max = max) |>
    mutate(results = pmap(list(mean, sd, n, min, max, verbose = FALSE), tides)) |>
    unnest(results)
}
