#' Plot TIDES consistency check
#'
#' @description Depending on the input, this function either plots a single
#'   reported mean/SD value with the TIDES envelope or multiple reported values
#'   sharing the same parameters. It shows:
#'
#' - The reported mean and SD values.
#' - The TIDES-consistent region as an envelope of SD bounds across the range of
#'   means.
#' - Shaded regions where reported values would be inconsistent.
#'
#' @details This plot helps visualize whether reported mean and standard
#'   deviation values are internally consistent with bounded measurement scales,
#'   using the TIDES (Test for Internal Data Error Sensitivity) method.
#'
#' @param res Data frame. Output of `tides()`, containing at minimum the columns
#'   `mean`, `sd`, `n`, `min`, `max`, `n_items`, `digits`, `calculate_min_sd`,
#'   `method`, and `tides_consistent`.
#' @param method String. Method to use for boundary calculation. One of
#'   `"exact"` (the default) and `"approximate"`.
#' @param text_size Numeric. Scaling factor for text elements. Default is `0.6`.
#' @param color_true String. Color for consistent points. Default is a green
#'   color.
#' @param color_false String. Color for inconsistent points. Default is a blue
#'   color.
#'
#' @return A ggplot object showing the TIDES boundary envelope, shaded
#'   inconsistency zones, and reported mean–SD points with legend reversed.
#'
#' @examples
#' \dontrun{
#'   tides(
#'     mean = 3.10,
#'     sd = 0.80,
#'     n = 1100,
#'     min = 1,
#'     max = 7,
#'     n_items = 1,
#'     digits = 2
#'   ) |>
#'     plot_tides()
#'
#'   tides(
#'     mean = 5.07,
#'     sd = 2.92,
#'     n = 15,
#'     min = 1,
#'     max = 7,
#'     n_items = 1,
#'     digits = 2
#'   ) |>
#'     plot_tides()
#'
#'   tides(
#'     mean = 5.07,
#'     sd = 2.92,
#'     n = 15,
#'     min = 1,
#'     max = 7,
#'     n_items = 1,
#'     digits = 2,
#'     method = "approximate"
#'   ) |>
#'     plot_tides()
#'
#'   tibble(
#'     mean = round_half_up(runif(n = 20, min = 1, max = 7), 2),
#'     sd = round_half_up(runif(n = 20, min = 0, max = 4), 2),
#'     n = 30,
#'     min = 1,
#'     max = 7,
#'     n_items = 1,
#'     digits = 2,
#'     calculate_min_sd = TRUE,
#'     verbose = FALSE,
#'     method = "approximate"
#'   ) |>
#'     mutate(
#'       results = pmap_dfr(
#'         list(
#'           mean = mean,
#'           sd = sd,
#'           n = n,
#'           min = min,
#'           max = max,
#'           n_items = n_items,
#'           digits = digits,
#'           calculate_min_sd = calculate_min_sd,
#'           verbose = verbose,
#'           method = method
#'         ),
#'         tides
#'       )
#'     ) |>
#'     unnest(results) |>
#'     plot_tides()
#' }

#'
#' @importFrom ggplot2 ggplot geom_polygon geom_line geom_point scale_color_manual scale_y_continuous scale_x_continuous theme_minimal theme guides guide_legend
#' @importFrom dplyr distinct filter mutate slice
#' @importFrom tidyr unnest expand_grid drop_na
#' @importFrom tibble tibble
#' @importFrom purrr pmap
#' @importFrom scales breaks_pretty
#'
#' @export
plot_tides <- function(
  res,
  method = c("exact", "approximate"),
  point_size = 1.5,
  text_size = 0.6,
  color_true = "#43BF71FF",
  color_false = "#35608DFF"
) {
  method <- rlang::arg_match(method)

  n_distinct_contexts <- res |>
    distinct(min, max, n_items, digits, method) |>
    nrow()

  if (n_distinct_contexts > 1) {
    cli::cli_abort(c(
      "Columns `min`, `max`, `n_items`, `digits`, and `method`
      must be identical on all rows.",
      "x" = "There are actually {n_distinct_contexts} distinct rows
      in terms of these values."
    ))
  }

  data_params <- dplyr::slice(res, 1)
  step_size <- 10^-data_params$digits

  boundary_data <- tidyr::expand_grid(
    mean = seq(
      from = data_params$min,
      to = data_params$max,
      by = step_size
    ),
    n = data_params$n,
    n_items = data_params$n_items,
    digits = data_params$digits,
    min = data_params$min,
    max = data_params$max,
    calculate_min_sd = data_params$calculate_min_sd
  ) |>
    dplyr::mutate(
      sd_bounds = purrr::pmap(
        list(mean, n, min, max, n_items, digits, calculate_min_sd),
        sd_bounds
      )
    ) |>
    tidyr::unnest(sd_bounds)

  if (method == "approximate") {
    boundary_data <- boundary_data |>
      dplyr::filter(mean >= min, mean <= max) |>
      approximate_sd_bounds()

    label_true <- "TIDES consistent"
    label_false <- "TIDES inconsistent"
  } else if (method == "exact") {
    boundary_data <- boundary_data |>
      tidyr::drop_na(min_sd, max_sd)

    if (!data_params$calculate_min_sd) {
      boundary_data <- boundary_data |>
        dplyr::mutate(min_sd = 0)
    }

    label_true <- "GRIMMER-TIDES consistent"
    label_false <- "GRIMMER-TIDES inconsistent"
  } else {
    cli::cli_abort("Internal error: unhandled `method` variant.")
  }

  nrow_boundary_data <- nrow(boundary_data)

  poly_above <- tibble::new_tibble(
    list(
      x = c(boundary_data$mean, rev(boundary_data$mean)),
      y = c(boundary_data$max_sd, rep(Inf, nrow_boundary_data))
    ),
    nrow = 2 * nrow_boundary_data
  )

  poly_below <- tibble::new_tibble(
    list(
      x = c(boundary_data$mean, rev(boundary_data$mean)),
      y = c(boundary_data$min_sd, rep(-Inf, nrow_boundary_data))
    ),
    nrow = 2 * nrow_boundary_data
  )

  poly_left <- tibble::new_tibble(
    list(
      x = c(-Inf, data_params$min, data_params$min, -Inf),
      y = c(-Inf, -Inf, Inf, Inf)
    ),
    nrow = 4
  )

  poly_right <- tibble::new_tibble(
    list(
      x = c(data_params$max, Inf, Inf, data_params$max),
      y = c(-Inf, -Inf, Inf, Inf)
    ),
    nrow = 4
  )

  step_span_5 <- step_size * 5

  # Build and return the plot
  ggplot() +
    geom_polygon(data = poly_above, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_below, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_left, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_polygon(data = poly_right, aes(x, y), fill = "grey10", alpha = 0.3) +
    geom_line(data = boundary_data, aes(x = mean, y = max_sd)) +
    geom_line(data = boundary_data, aes(x = mean, y = min_sd)) +
    geom_point(
      data = res,
      aes(mean, sd, color = tides_consistent),
      size = point_size
    ) +
    scale_color_manual(
      values = c("TRUE" = color_true, "FALSE" = color_false),
      labels = c("TRUE" = label_true, "FALSE" = label_false)
    ) +
    scale_y_continuous(
      name = "Standard Deviation",
      limits = c(0, NA),
      breaks = scales::breaks_pretty(n = 8),
      expand = c(step_span_5, step_span_5)
    ) +
    scale_x_continuous(
      name = "Mean",
      breaks = scales::breaks_pretty(n = 10),
      expand = c(step_span_5, step_span_5)
    ) +
    theme_minimal(base_size = text_size * 20) +
    theme(legend.position = "top") +
    guides(
      color = guide_legend(
        reverse = TRUE,
        override.aes = list(size = 4, ncol = 1),
        title = NULL
      )
    )
}
