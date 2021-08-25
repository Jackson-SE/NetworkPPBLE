#' Calculate Euclidean distance between two vectors
#'
#' @param x1 vector
#' @param x2 vector
#'
#' @return scalar output with the distance between the two vectors.
#' @export
#'
#' @examples
#' a <- c( 3, 5, 6.2 )
#' b <- c( 1, 9, 3.1 )
#' euclidean_distance( a, b )
euclidean_distance <- function( x1, x2 ){

  # Check input conditions.
  if( ( is.vector( x1 ) & is.vector( x2 ) ) == FALSE ){ stop( "x1 and x2 must both be vectors." ) }

  # Calculate and return the euclidean distance between the two vectors.
  sqrt( sum( ( x1 - x2 )^2 ) )

}
