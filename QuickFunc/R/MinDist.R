#' MinDist
#'
#'@description Computes the minimum distance between the points (rows) of a matrix.
#'
#' @param X matrix or dataframe
#'
#' @return minimum distance between any two of the points in (given by the rows of) X
#' @export
#'
#' @examples
#' X <- matrix( runif( 12 ), ncol = 3 )
#' MinDist( X )
#' Y <- data.frame( X, row.names = c( "cat", "dog", "rabbit", "mouse" ) )
#' MinDist( Y )
MinDist <- function( X ){

  # Check the input conditions.
  if( ( is.matrix( X ) | is.data.frame( X ) ) == FALSE ){ stop( "X must be a matrix or a dataframe." ) }

  # Run the command.
  min( as.vector( stats::dist( X ) ) )

}
