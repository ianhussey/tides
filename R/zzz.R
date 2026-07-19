#' tides: Truncation-Induced Dependency among Summary Statistics
#'
#' A forensic meta-science / trustworthiness-assessment toolkit for checking
#' whether reported means, standard deviations and sample sizes measured on a
#' bounded (truncated) scale are mutually consistent. When a measure has a known
#' minimum and maximum, the mean it can take constrains the standard deviation
#' that is arithmetically possible: the two summary statistics are dependent.
#' \code{tides()} computes the feasible SD range for a reported mean, sample
#' size and scale range and flags reports that fall outside it. Companion
#' functions apply the check across a data frame (\code{tides_df()}), visualise
#' the feasible envelope (\code{plot_tides()}, \code{plot_tides_relative()}),
#' and enumerate the jointly GRIM-, GRIMMER- and TIDES-consistent grid of
#' mean-SD pairs (\code{umbrella()}, \code{plot_umbrella()}).
#'
#' @seealso The package README and vignette (\code{vignette("tides")}) for a
#'   worked example and the method background.
#' @keywords internal
"_PACKAGE"
