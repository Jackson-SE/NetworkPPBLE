#' Scale points
#'
#' @description Scale point from one hypercuboid domain space to another.
#'
#' @param x vector, matrix or dataframe of points (given by the elements or rows respectively) of points to scale.
#' @param a vector of lower limits of the original space, one for each dimension (column of x)
#' If all the lower limits are the same, that scalar value can be given.
#' @param b vector of upper limits of the original space, one for each dimension (column of x)
#' If all the upper limits are the same, that scalar value can be given.
#' @param l vector of lower limits of the transformed space, one for each dimension (column of x)
#' If all the lower limits are the same, that scalar value can be given.
#' @param u vector of upper limits of the transformed space, one for each dimension (column of x)
#' If all the upper limits are the same, that scalar value can be given.
#'
#' @details Scales the vector of matrix of points \code{x} from the hypercuboid [a,b] to [l,u].
#'
#' @return a vector or matrix of the transformed points.
#' @export
#'
#' @examples
#' X <- matrix( runif(15, 2, 4), ncol = 3 )
#' Scale( X, a = 2, b = 4 )
#' # Compare with:
#' X - 3
Scale <- function(x, a = 0, b = 1, l = -1, u = 1) {

  if( ( is.matrix( x ) | is.vector( x ) | is.data.frame( x ) ) == FALSE ){ stop( "x must be a vector, matrix or dataframe." ) }
  if( length( a ) != 1 & length( a ) != NCOL( x ) ){ stop( "a must be a vector of length 1 or equal to number of columns of x" ) }
  if( length( b ) != 1 & length( b ) != NCOL( x ) ){ stop( "b must be a vector of length 1 or equal to number of columns of x" ) }
  if( length( l ) != 1 & length( l ) != NCOL( x ) ){ stop( "l must be a vector of length 1 or equal to number of columns of x" ) }
  if( length( u ) != 1 & length( u ) != NCOL( x ) ){ stop( "u must be a vector of length 1 or equal to number of columns of x" ) }

  t( (u - l) * ((t( x ) - a)/(b - a)) + l )

}
