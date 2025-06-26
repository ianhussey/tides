#' Compute approximate SD bounds by filling from nearby means' SD bounds
#'
#' \code{approximate_sd_bounds} applies an interpolation strategy to
#' relax discontinuities in SD boundary curves produced by \code{sd_bounds()}.
#' It combines downward and upward filled versions of the SD limits from nearby 
#' means and returns a smoothed boundary envelope for each mean value. This is 
#' intended to be an internal function mostly for use inside \code{plot_tides}.
#'
#' @param dat A data frame containing the variables \code{mean}, \code{min_sd},
#'   \code{max_sd}, \code{min}, \code{max}, \code{n}, and \code{n_items}, such
#'   as the output of a call to \code{sd_bounds()} over a grid of means.
#'
#' @return A data frame with one row per unique mean value (and associated
#'   metadata), containing relaxed lower and upper SD bounds (\code{min_sd},
#'   \code{max_sd}).
#'
#' @export
approximate_sd_bounds <- function(dat) {
  dat |>
    arrange(mean) |>
    # 1) make downward-filled versions
    mutate(min_sd_down = min_sd,
           max_sd_down = max_sd) |>
    tidyr::fill(min_sd_down, max_sd_down, .direction = "down") |>
    # 2) make upward-filled versions
    mutate(min_sd_up = min_sd,
           max_sd_up = max_sd) |>
    tidyr::fill(min_sd_up, max_sd_up, .direction = "up") |>
    # 3) take the strictest lower bound and the loosest upper bound
    mutate(min_sd_filled = pmin(min_sd_down, min_sd_up, na.rm = TRUE),
           max_sd_filled = pmax(max_sd_down, max_sd_up, na.rm = TRUE)) |>
    # 4) collapse back to one row per unique mean (and its metadata)
    group_by(mean, min, max, n, n_items) |>
    summarize(min_sd = min(min_sd_filled, na.rm = TRUE),
              max_sd = max(max_sd_filled, na.rm = TRUE),
              .groups = "drop")
}
