#' Bayes Linear Emulator with Sampling.
#'
#' @param object an object of the type \code{emulate}.
#' @param EX vector or matrix of expected values.
#' @param VarX vector or matrix of variances, note that this should be the same dimension as \code{EX} as each input is sampled independently at present (this is largely an efficiency consideration).
#' @param n_samples number of samples that should be sampled for each expectation and variance provided.
#' @param sampling_function choice of sampling function, defaulting to normal.
#' @param n_sd number of standard deviations to be used when sampling according to the uniform distribution.  Using code{sqrt(3)}, as if default, makes the uniform distribution have the mean and variance specified.
#' @param batch_size the size of the batches at which to perform the predictions, default is 100.
#'
#' @return
#' \item{Ehx_hat}{BL adjusted expectation for h(x).}
#' \item{Varhx_hat}{BL adjusted variance for h(x).}
#' @export
#'
#' @details This function performs Bayes linear emulation with sampling.  An uncertain input provided by each element in the vector or matrices \code{EX} and \code{VarX} is used to generate a sample from a chosen distribution with corresponding second-order specification.  Each sample is run through the \code{\link{predict.emulate}} function, before an expected value and variance is then calculated over all of the outputs from a specific sample.
#'
#' @examples
#' f <- function( x ){ c( x[2] * sin( x[1] ) + x[1] * cos( x[2] ), 2 * x[1] + 3 * x[2] / x[1] ) }
#' x <- matrix( runif( 20,0.2,1.2 ), ncol = 2 )
#' fx <- t( apply(x, 1, f) )
#' theta <- c( 0.4, 0.6 )
#' emulator <- emulate( x = x, fx = fx, CF = GaussianCF,
#'                      CF_para = list( theta = theta, delta = 0.0001 )  )
#' E_Y <- matrix( runif( 16, 0.2, 1.2), ncol = 2 )
#' Var_Y <- matrix( rep( 0.01, 16 ), ncol = 2 )
#' BLESampling( emulator, E_Y, Var_Y )
#' BLESampling( emulator, E_Y, Var_Y, sampling_function = "uniform" )
BLESampling <- function( object, EX, VarX, n_samples = 100, sampling_function = "normal", n_sd = sqrt( 3 ), batch_size = 100 ){

  # Check input conditions.
  if( is.na( match( "emulate", class( object ) ) ) ){ stop( "object is expected to be of class emulate." ) }
  if( identical( NROW( EX ), NROW( VarX ) ) == FALSE | identical( NCOL( EX ), NCOL( VarX ) ) == FALSE ){
    stop( "The dimension of EX and VarX should be the same." ) }
  if( NCOL( EX ) != NCOL( object$xv ) ){ stop( "The number of columns of EX and VarX should be the same as that of the emulator training points xv." ) }

  # Number of prediction points.
  n <- NROW( EX )

  # Number of outputs of EX.
  l <- NCOL( EX )

  # Number of output components (note this is the output to h, not f).
  k <- NCOL( object$fxv )

  # Take the correct sample.
  if( identical( sampling_function, "normal" ) ){

    # This gives an object of dimension n_samples x l x n (where l is the number of columns of EX).
    fx_sample <- QuickFunc::MultNormSamples( Means = t( EX ), Variances = t( VarX ), n = n_samples )

    # This gives an object of dimension n_samples x n x l then n_samples*n (n sets of n_samples) x l
    fx_sample <- matrix( aperm( fx_sample, perm = c(1,3,2) ), nrow = n_samples * n )

  }

  if( identical( sampling_function, "uniform" ) ){

    # Obtain a sample n*l*n_samples
    fx_sample <- stats::runif( n = n*l*n_samples, min = EX - n_sd * sqrt( VarX ), max = EX + n_sd * sqrt( VarX ) )

    # First put into an array that is n x l x n_samples.
    fx_sample <- array( fx_sample, dim = c( n, l, n_samples ) )

    # This gives an object of dimension n_samples x n x l then n_samples*n (n sets of n_samples) x l
    fx_sample <- matrix( aperm( fx_sample, perm = c(3,1,2) ), nrow = n_samples * n )

  }

  # Run the emulator at the sampled points - giving an object of size n_samples*n x k.
  gzu_BLsample <- stats::predict( object = object,
                                  x = fx_sample,
                                  batch_size = batch_size )

  # Convert the results to n*k x n_samples matrices.
  Efx_Mat <- matrix( gzu_BLsample$Efx, ncol = n_samples, byrow = TRUE )
  Varfx_Mat <- matrix( gzu_BLsample$Varfx, ncol = n_samples, byrow = TRUE )

  # Calculate the Means and Variances over each of the n_samples samples.
  Efx_Means <- rowMeans( Efx_Mat )
  Varfx_Means <- rowMeans( Varfx_Mat )
  Efx_Variances <- rowSums( ( Efx_Mat - Efx_Means )^2 ) / ( n_samples - 1 )

  # Finally, put the results together into matrices of the required sizes.
  EfX_hat <- matrix( Efx_Means, nrow = n )
  VarfX_hat <- matrix( Efx_Variances + Varfx_Means, nrow = n )

  # Return the results.
  return( list( "EfX" = EfX_hat, "VarfX" = VarfX_hat ) )

}
