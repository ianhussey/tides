% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_tides_single.R
\name{plot_tides_single}
\alias{plot_tides_single}
\title{Calculate and plot a TIDES test for a single set of values}
\usage{
plot_tides_single(res, text_size = 0.6)
}
\arguments{
\item{res}{a data frame, the output of tides_single().}

\item{base_size}{scaling for the text in the plot.}
}
\value{
A ggplot object: the TIDES plot
}
\description{
Explanation to be added
}
\examples{
\dontrun{
tides_single(mean = 3.10, sd = 0.80, n = 1100, min = 1, max = 7, n_items = 1, digits = 2) |>
  plot_tides_single()

tides_single(mean = 5.07, sd = 2.92, n = 15, min = 1, max = 7, n_items = 1, digits = 2) |>
  plot_tides_single()
}

}
