#' Apply the `tides()` function to each row of a data frame
#'
#' `tides_df()` takes a data frame (.data) containing columns for
#' mean, standard deviation, sample size, minimum, maximum,
#' number of items, display digits, and a flag for whether to
#' recalculate the minimum standard deviation.  It then applies
#' your existing `tides()` function to each row and binds the
#' results into the original data frame.
#'
#' @param .data A data frame or tibble.  Must contain the columns "mean",
#'   "sd", "n", "min" and "max" (unless supplied via the arguments below).
#'   The optional columns "n_items", "digits", "calculate_min_sd" and "method"
#'   are used when present and otherwise fall back to the \code{tides()}
#'   defaults, so they need not be included.
#' @param mean Optional.  A numeric vector or a column reference
#'   in `.data` providing the group means.  Defaults to the
#'   `.data$mean` column.
#' @param sd Optional.  A numeric vector or a column reference
#'   in `.data` providing the standard deviations.  Defaults to
#'   `.data$sd`.
#' @param n Optional.  A numeric vector or a column reference
#'   in `.data` providing the sample sizes.  Defaults to
#'   `.data$n`.
#' @param min Optional.  A numeric vector or a column reference
#'   in `.data` providing the observed minima.  Defaults to
#'   `.data$min`.
#' @param max Optional.  A numeric vector or a column reference
#'   in `.data` providing the observed maxima.  Defaults to
#'   `.data$max`.
#' @param n_items Optional.  A numeric vector or a column
#'   reference for the number of items in the scale.  Defaults
#'   to `.data$n_items`. Note: values above 1 are not yet supported and are
#'   currently forced to 1 internally (see "Limitations" in the README).
#' @param digits Optional.  An integer vector or a column
#'   reference indicating how many decimal places to display.
#'   Defaults to `.data$digits`.
#' @param calculate_min_sd Optional.  A logical vector or a
#'   column reference indicating whether `tides()` should recalculate
#'   a minimum standard deviation.  Defaults to
#'   `.data$calculate_min_sd`.
#' @param method Optional.  A string vector or a
#'   column reference indicating whether `tides()` should test the 
#'   strict SD bounds for only the supplied mean (i.e., implicitly 
#'   test for GRIM and GRIMMER consistency, when argument is set to 
#'   'exact'), or whether in the case of undefined SD bounds the 
#'   bounds of nearby means should be used (when argument is set to 
#'   'approximate').
#' @param approximate_bounds_range Integer. Passed to `tides()`; the number of
#'   precision steps in either direction to search for a nearby mean with valid
#'   SD bounds when `method = "approximate"`. Defaults to 10.
#'
#' @return A tibble combining the original `.data` with the
#'   output of `tides()` for each row.  The result will include
#'   any columns returned by `tides()`, un-nested into regular
#'   data-frame columns.
#'
#' @details
#' This function uses tidy-evaluation to allow unquoted column
#' names when piping.  If you omit any of the mapping arguments,
#' it will assume a column of the same name exists in `.data`.
#'
#' Internally, `purrr::pmap_dfr()` applies `tides()` row by row,
#' then `tidyr::unnest()` expands the results into a standard
#' data-frame form.
#'
#' @examples
#' dat <- data.frame(
#'   mean             = c(4.2, 4.2, 1.2, 1.4),
#'   sd               = c(0.5, 0.5, 0.5, 0.6),
#'   n                = c( 14,  14,  30,  35),
#'   min              = 1,
#'   max              = c(7, 7, 5, 7),
#'   n_items          = 1,
#'   digits           = 2,
#'   calculate_min_sd = TRUE,
#'   method           = c("exact", "approximate", "exact", "exact")
#' )
#'
#' res <- tides_df(dat)
#' print(res)
#'
#' @importFrom dplyr mutate
#' @importFrom purrr pmap_dfr
#' @importFrom tidyr unnest
#' @importFrom rlang enquo quo_is_null quo
#' @export
tides_df <- function(.data,
                     mean             = NULL,
                     sd               = NULL,
                     n                = NULL,
                     min              = NULL,
                     max              = NULL,
                     n_items          = NULL,
                     digits           = NULL,
                     calculate_min_sd = NULL,
                     method           = NULL,
                     approximate_bounds_range = 10) {
  
  # currently n_items is hard coded to 1 while i figure out how to implement it
  n_items <- 1
  
  # capture each argument as a quosure
  mean_q   <- enquo(mean)
  sd_q     <- enquo(sd)
  n_q      <- enquo(n)
  min_q    <- enquo(min)
  max_q    <- enquo(max)
  items_q  <- enquo(n_items)
  digs_q   <- enquo(digits)
  calc_q   <- enquo(calculate_min_sd)
  method_q <- enquo(method)

  # If the user did not supply a required argument, point its quosure at the
  # column of the same name (an error is raised later if it is absent).
  if (quo_is_null(mean_q)) mean_q <- quo(mean)
  if (quo_is_null(sd_q))   sd_q   <- quo(sd)
  if (quo_is_null(n_q))    n_q    <- quo(n)
  if (quo_is_null(min_q))  min_q  <- quo(min)
  if (quo_is_null(max_q))  max_q  <- quo(max)

  # For optional arguments, fall back to the column of the same name when it
  # exists, otherwise to the tides() default, so a data frame need not carry
  # these columns.
  if (quo_is_null(items_q)) {
    items_q <- if ("n_items" %in% names(.data)) quo(n_items) else quo(1)
  }
  if (quo_is_null(digs_q)) {
    digs_q <- if ("digits" %in% names(.data)) quo(digits) else quo(2)
  }
  if (quo_is_null(calc_q)) {
    calc_q <- if ("calculate_min_sd" %in% names(.data)) quo(calculate_min_sd) else quo(TRUE)
  }
  if (quo_is_null(method_q)) {
    method_q <- if ("method" %in% names(.data)) quo(method) else quo("exact")
  }

  res <- .data |>
    mutate(results = pmap_dfr(.l = list(mean             = !!mean_q,
                                        sd               = !!sd_q,
                                        n                = !!n_q,
                                        min              = !!min_q,
                                        max              = !!max_q,
                                        n_items          = !!items_q,
                                        digits           = !!digs_q,
                                        calculate_min_sd = !!calc_q,
                                        method           = !!method_q,
                                        approximate_bounds_range = approximate_bounds_range),
                              verbose = FALSE,
                              .f = tides)) |>
    unnest(results)
  
  return(res)
}
