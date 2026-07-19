# Declare variables used in non-standard (tidy) evaluation so that
# R CMD check does not raise "no visible binding for global variable" NOTEs.
utils::globalVariables(c(
  # tides() / sd_bounds() / tides_df()
  "mean", "sd", "n", "min", "max", "n_items", "digits", "precision",
  "calculate_min_sd", "method", "results", "sd_bounds",
  "min_sd", "max_sd", "tides_consistent",
  "sd_range_calculable", "mean_inside_range", "sd_inside_range", "inside_ranges",
  # approximate_sd_bounds()
  "min_sd_down", "max_sd_down", "min_sd_up", "max_sd_up",
  "min_sd_filled", "max_sd_filled",
  # umbrella()
  "grimmer", "mean_char", "sd_char", "rounding",
  # plotting
  "relative_location", "relative_dispersion",
  "x", "y", "xend", "yend", "xmin", "xmax", "ymin", "ymax"
))
