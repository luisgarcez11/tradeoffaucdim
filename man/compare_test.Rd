% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/06_compare_test.R
\name{compare_test}
\alias{compare_test}
\title{Compare test}
\usage{
compare_test(obj, x_label_offset = 1, y_label_offset = 10)
}
\arguments{
\item{obj}{object returned by \code{plot_curve}}

\item{x_label_offset}{x coordinate to plot p-value}

\item{y_label_offset}{y coordinate to plot p-value}
}
\value{
list with statistical tests performed
}
\description{
Performs statistical tests to compare performance and runtime.
}
\examples{
compare_test(obj5)
}
