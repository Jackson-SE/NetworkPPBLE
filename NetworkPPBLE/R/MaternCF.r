#' Matern Correlation Function
#'
#' @description Calculate the Gaussian correlation function between the points in (given by the rows of) two matrices.
#'
#' @param X a vector, matrix or dataframe
#' @param Y a vector, matrix or dataframe
#' @param kappa range paraneter
#' @param nu shape parameter
#'
#' @return Matern correlation function value between the rows of X and Y,
#' given as a matrix of dimension \code{nrow(X)} by \code{nrow(Y)}.
#' @export
#'
#' @examples
#' X <- matrix( rnorm( 10 ), ncol = 2 )
#' Y <- matrix( runif( 6 ), ncol = 2 )
#' kappa <- 10
#' nu <- 1/5
#' MaternCF( X, Y, kappa, nu )
#' MaternCF( as.data.frame(X), Y, kappa, nu )
MaternCF <- function( X, Y = X, kappa, nu ){

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

  # Convert a dataframe to a matrix if necessary.
  if( is.data.frame( X ) ){ X <- as.matrix( X ) }
  if( is.data.frame( Y ) ){ Y <- as.matrix( Y ) }

  # Compute the Matern correlation between each row of X and each row of Y.  Check first whether X is identical to Y.
  # If so, then use function dist instead of pdist to avoid R returning an error.
  if( identical( X, Y ) ){
    CorrX <- rSPDE::matern.covariance( as.matrix( stats::dist( X ) ), kappa = kappa, nu = nu, sigma = 1 )
    return( CorrX )
  }else{
    CorrXY <- rSPDE::matern.covariance( as.matrix( pdist::pdist( X, Y ) ), kappa = kappa, nu = nu, sigma = 1 )
    return( CorrXY )
  }

}
