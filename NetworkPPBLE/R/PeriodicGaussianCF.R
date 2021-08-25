#' Periodic Gaussian Correlation Function
#'
#' @description Calculate the Gaussian correlation function between the points in (given by the rows of) two matrices, given that
#' there is periodicity in the values of (some of) the inputs.
#'
#' @inheritParams GaussianCF
#' @param p vector of period values for each of the inputs.  If there is no period then the corresponding element
#' of this vector should be given as \code{NA}.
#'
#' @return Periodic Gaussian correlation function value between the rows of X and Y,
#' given as a matrix of dimension \code{nrow(X)} by \code{nrow(Y)}.
#' @export
#'
#' @seealso
#' \code{\link{GaussianCF}}
#'
#' @examples
#' X <- matrix( rnorm( 10 ), ncol = 2 )
#' Y <- matrix( runif( 6 ), ncol = 2 )
#' theta <- c( 0.5, 0.8 )
#' GaussianCF( X, Y, theta = theta )
#' # if p is not specified, then the results should be the same as above.
#' PeriodicGaussianCF( X, Y, theta = theta )
#' # compare if we have a period of 1 for each input.
#' PeriodicGaussianCF( X, Y, theta = theta, p = rep( 1, 2 ) )
#' # or a period for just one of the inputs.
#' PeriodicGaussianCF( X, Y, theta = theta, p = c( NA, 0.5 ) )
PeriodicGaussianCF <- function( X, Y = X, theta, p = rep( NA, NCOL( X ) ), delta = 0 ){

  # If X and Y are vectors, then we turn them into a matrix with a single column.
  if( is.vector( X ) == TRUE ){
    X <- matrix( X, ncol = 1 )
  }
  if( is.vector( Y ) == TRUE ){
    Y <- matrix( Y, ncol = 1 )
  }

  # Check input conditions.
  if( ( is.matrix( X ) | is.data.frame( X ) ) == FALSE ){ stop( "X must be a vector, matrix or dataframe." ) }
  if( ( is.matrix( Y ) | is.data.frame( Y ) ) == FALSE ){ stop( "Y must be a vector, matrix or dataframe." ) }
  if( ncol( X ) != ncol( Y ) ){ stop( "X and Y must have the same number of columns." ) }
  if( length( theta ) != ncol( X ) ){ stop( "theta must be the same length as the number of columns of X." ) }
  if( ( is.vector( delta ) & length( delta ) == 1 & delta >= 0 ) == FALSE ){ stop( "delta must be a non-negative scalar" ) }
  if( length( p ) != ncol( X ) ){ stop( "p must be the same length as the number of columns of X." ) }

  # Convert a dataframe to a matrix if necessary.
  if( is.data.frame( X ) ){ X <- as.matrix( X ) }
  if( is.data.frame( Y ) ){ Y <- as.matrix( Y ) }

  # Relevant dimensions.
  nx <- NROW( X )
  ny <- NROW( Y )
  d <- NCOL ( X )

  # Generate an empty matrix to store the results.
  pD <- matrix( NA, nrow = nx * ny, ncol = d )

  # Compute the inside of the exponential part of the periodic Gaussian correlation of each of the
  # d inputs between each row of X and each row of Y.  Store as the nx*ny by d matrix defined above.
  for( i in 1:d ){

    # if period p is NA, then we are just calculating a standard Gaussian CF.
    if( is.na( p[i] ) ){

      pD[,i] <- ( c( outer( X[,i], Y[,i], FUN = "-" ) ) / theta[i] )^2

    }else{

      # otherwise we want to calculate the periodic version of the Gaussian CF.
      pD[,i] <-  ( c( QuickFunc::periodic_dist( X = X[,i], Y = Y[,i], p = p[i] ) ) / theta[i] )^2

    }

  }

  # Now we want to sum each row of that matrix, and take the exponential of the negative of it, before storing the results in an
  # nx by ny matrix.
  # This will allow us to compute the Gaussian correlation between each row of X and each row of Y.
  # Check first whether X is identical to Y.
  # If so, then use function dist instead of pdist to avoid R returning an error.
  if( identical( X, Y ) ){
    CorrX <- ( 1 - delta ) * matrix( exp( - rowSums( pD ) ), nrow = nx, ncol = ny ) + delta * diag( nrow( X ) )
    return( CorrX )
  }else{
    CorrXY <- ( 1 - delta ) * matrix( exp( - rowSums( pD ) ), nrow = nx, ncol = ny )
    return( CorrXY )
  }

}
