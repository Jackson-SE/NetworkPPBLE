#' Gaussian Correlation Function
#'
#' @description Calculate the Gaussian correlation function between the points in (given by the rows of) two matrices.
#'
#' @param X a vector, matrix or dataframe
#' @param Y a vector, matrix or dataframe
#' @param theta a vector of correlation length parameter values (one for each column of X).
#' @param delta an (optional) scalar nugget parameter.
#'
#' @return Gaussian correlation function value between the rows of X and Y,
#' given as a matrix of dimension \code{nrow(X)} by \code{nrow(Y)}.
#' @export
#'
#' @examples
#' X <- matrix( rnorm( 10 ), ncol = 2 )
#' Y <- matrix( runif( 6 ), ncol = 2 )
#' theta <- c( 0.5, 0.8 )
#' GaussianCF( X, Y, theta )
#' GaussianCF( as.data.frame(X), Y, theta )
GaussianCF <- function( X, Y = X, theta, delta = 0 ){

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

  # Convert a dataframe to a matrix if necessary.
  if( is.data.frame( X ) ){ X <- as.matrix( X ) }
  if( is.data.frame( Y ) ){ Y <- as.matrix( Y ) }

  # Compute the Gaussian correlation between each row of X and each row of Y.  Check first whether X is identical to Y.
  # If so, then use function dist instead of pdist to avoid R returning an error.
  if( identical( X, Y ) ){
    CorrX <- ( 1 - delta ) * exp( - as.matrix( stats::dist( QuickFunc::AVEM( X, 1 / theta ) ) )^2 ) + delta * diag( nrow( X ) )
    return( CorrX )
  }else{
    CorrXY <- ( 1 - delta ) * exp( - as.matrix( pdist::pdist( QuickFunc::AVEM( X, 1 / theta ), QuickFunc::AVEM( Y, 1 / theta ) ) )^2 )
    return( CorrXY )
  }

}
