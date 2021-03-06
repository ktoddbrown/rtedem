% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/measureFunctions.R
\name{RMSE.measure}
\alias{RMSE.measure}
\title{Root mean squared error}
\usage{
RMSE.measure(model, data, ...)
}
\arguments{
\item{model}{numeric not associated with mean-standard deviations, likely model results}

\item{data}{numeric mean associated with a standard devation, likely data}

\item{...}{ignore other parameters passed to this function}
}
\value{
a numeric with the root mean squared error between model and mean. \eqn{\sqrt{\frac{\sum{(m-d)^2}}{n}}}
}
\description{
Calculate the root mean squared difference between two paired numerics. Via the square root of, the sum of the squared difference divided by the number of pairs.
}

