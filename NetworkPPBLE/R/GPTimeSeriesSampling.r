#' Generate a psuedo-GP sample from a BLE time series.
#'
#' @param Efx Expected value of f(x) across time.
#' @param Varfx Variance of f(x) across time.
#' @param times Values of time (t) for which BLE was evaluated.
#' @param theta Correlation length parameter across time.
#' @param subtimes Vector of subtimes for the purposes of sampling.  Useful if the length of times makes MVN sampling too computationally intensive.
#' @param nz Number of samples required for each x-value.
#' @param non_negative if true, zeroes any negative samples.
#'
#' @return A matrix of sampled time series, with \code{nz} samples for each row of \code{Efx}.
#' @export
#'
#' @examples
#' beta_sample <- runif( 5 )
#' times <- seq( from = 0 , to = 1, by = 0.1 )
#' Efx <- kronecker( beta_sample, t( times ) )
#' Varfx <- kronecker( rep( 1, 5 ), t( ( 0.5 - times )^2 ) )
#' theta <- 1
#' GPsamp <- GPTimeSeriesSampling( Efx = Efx, Varfx = Varfx, times = times, theta = theta )
#' par( mfrow = c(1, 1) )
#' graphics::matplot( times, t( GPsamp ), type = "l", lty = 1 )
GPTimeSeriesSampling <- function( Efx, Varfx, times, theta, subtimes = times, nz = 10, non_negative = FALSE ){

  # Number of time series.
  n <- NROW( Efx )

  # nz is the number of samples per provided Efx and Varfx specification.
  # N is therefore the total number of samples we need.
  N <- nz * n

  # Covariance matrix using Gaussian CF (to make it smooth) above.
  TK <- NetworkPPBLE::GaussianCF( subtimes, subtimes, theta = theta, delta = 0.0001 )

  # Sample using a MV normal distribution.  Note that here the samples are centred around zero.
  Tsam <- MASS::mvrnorm( n = N, mu = rep( 0, length = length( subtimes ) ), Sigma = TK )

  # Interpolation - required if times is not equivalent to subtimes.
  if( identical( times, subtimes ) ){
    TsamInterp <- t( Tsam )
  }else{
    TsamInterp <- QuickFunc::kNNinterp( x = times, y = subtimes, z = t( Tsam ) )
  }

  # Now we need to combine the time correlation with the predictions!
  repeats <- rep( 1:n, each = nz )

  # GP samples.
  GPsamples <- Efx[repeats,] + sqrt( Varfx[repeats,] ) * t( TsamInterp )

  # Zero any negative samples.
  if( non_negative == TRUE ){ GPsamples[GPsamples < 0] <- 0 }

  # Return GPsamples
  return( GPsamples )

}
