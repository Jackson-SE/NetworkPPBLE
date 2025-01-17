% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BLESampling.R
\name{BLESampling}
\alias{BLESampling}
\title{Bayes Linear Emulator with Sampling.}
\usage{
BLESampling(
  object,
  EX,
  VarX,
  n_samples = 100,
  sampling_function = "normal",
  n_sd = sqrt(3),
  batch_size = 100
)
}
\arguments{
\item{object}{an object of the type \code{emulate}.}

\item{EX}{vector or matrix of expected values.}

\item{VarX}{vector or matrix of variances, note that this should be the same dimension as \code{EX} as each input is sampled independently at present (this is largely an efficiency consideration).}

\item{n_samples}{number of samples that should be sampled for each expectation and variance provided.}

\item{sampling_function}{choice of sampling function, defaulting to normal.}

\item{n_sd}{number of standard deviations to be used when sampling according to the uniform distribution.  Using code{sqrt(3)}, as if default, makes the uniform distribution have the mean and variance specified.}

\item{batch_size}{the size of the batches at which to perform the predictions, default is 100.}
}
\value{
\item{Ehx_hat}{BL adjusted expectation for h(x).}
\item{Varhx_hat}{BL adjusted variance for h(x).}
}
\description{
Bayes Linear Emulator with Sampling.
}
\details{
This function performs Bayes linear emulation with sampling.  An uncertain input provided by each element in the vector or matrices \code{EX} and \code{VarX} is used to generate a sample from a chosen distribution with corresponding second-order specification.  Each sample is run through the \code{\link{predict.emulate}} function, before an expected value and variance is then calculated over all of the outputs from a specific sample.
}
\examples{
f <- function( x ){ c( x[2] * sin( x[1] ) + x[1] * cos( x[2] ), 2 * x[1] + 3 * x[2] / x[1] ) }
x <- matrix( runif( 20,0.2,1.2 ), ncol = 2 )
fx <- t( apply(x, 1, f) )
theta <- c( 0.4, 0.6 )
emulator <- emulate( x = x, fx = fx, CF = GaussianCF,
                     CF_para = list( theta = theta, delta = 0.0001 )  )
E_Y <- matrix( runif( 16, 0.2, 1.2), ncol = 2 )
Var_Y <- matrix( rep( 0.01, 16 ), ncol = 2 )
BLESampling( emulator, E_Y, Var_Y )
BLESampling( emulator, E_Y, Var_Y, sampling_function = "uniform" )
}
