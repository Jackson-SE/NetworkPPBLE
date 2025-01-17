% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GPTimeSeriesSampling.r
\name{GPTimeSeriesSampling}
\alias{GPTimeSeriesSampling}
\title{Generate a psuedo-GP sample from a BLE time series.}
\usage{
GPTimeSeriesSampling(
  Efx,
  Varfx,
  times,
  theta,
  subtimes = times,
  nz = 10,
  non_negative = FALSE
)
}
\arguments{
\item{Efx}{Expected value of f(x) across time.}

\item{Varfx}{Variance of f(x) across time.}

\item{times}{Values of time (t) for which BLE was evaluated.}

\item{theta}{Correlation length parameter across time.}

\item{subtimes}{Vector of subtimes for the purposes of sampling.  Useful if the length of times makes MVN sampling too computationally intensive.}

\item{nz}{Number of samples required for each x-value.}

\item{non_negative}{if true, zeroes any negative samples.}
}
\value{
A matrix of sampled time series, with \code{nz} samples for each row of \code{Efx}.
}
\description{
Generate a psuedo-GP sample from a BLE time series.
}
\examples{
beta_sample <- runif( 5 )
times <- seq( from = 0 , to = 1, by = 0.1 )
Efx <- kronecker( beta_sample, t( times ) )
Varfx <- kronecker( rep( 1, 5 ), t( ( 0.5 - times )^2 ) )
theta <- 1
GPsamp <- GPTimeSeriesSampling( Efx = Efx, Varfx = Varfx, times = times, theta = theta )
par( mfrow = c(1, 1) )
graphics::matplot( times, t( GPsamp ), type = "l", lty = 1 )
}
