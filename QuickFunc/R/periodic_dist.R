#' Periodic Difference
#'
#' @description Calculates the difference between two values under a particular period mod value.
#'
#' @param X a vector
#' @param Y a vector
#' @param p scalar value of period
#'
#' @return difference between the values in \code{X} and \code{Y} mod \code{p}, given as a \code{length( X )} by \code{length( Y )} matrix.
#' @export
#'
#' @examples
#' periodic_dist( runif( 3 ), runif( 4 ) )
periodic_dist <- function( X, Y = X, p = 1 ){

  # Check input conditions.
  if( is.vector( X ) == FALSE ){ stop( "X must be a vector." ) }
  if( is.vector( Y ) == FALSE ){ stop( "Y must be a vector." ) }
  if( ( is.vector( p ) & length( p ) == 1 ) == FALSE ){ stop( "p must be a scalar." ) }

  # Calculate the distance between each element of X and each element of Y.
  D <- outer( X, Y, FUN = "-" )

  # Now we need to calculate the periodic distance according to the period p.
  pD <- abs( ( D + p / 2 ) %% p - p / 2 )

  # Return the result.
  return( pD )

}
