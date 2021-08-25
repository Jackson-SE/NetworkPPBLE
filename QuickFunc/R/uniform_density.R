#' Multivariate Uniform Density (over a hypercube)
#'
#' @param x vector or matrix
#' @param l vector of lower bounds
#' @param u vector of upper bounds
#'
#' @return density of x for the multivariate uniform distribution specified by l and u.
#' @export
#'
#' @examples
#' x <- c(3, 4)
#' l <- c(2.3, 4.5)
#' u <- c(6, 6)
#' uniform_density( x, l, u )
#' uniform_density( x + 1, l, u )
uniform_density <- function( x, l, u ){

  # Check if x is a vector or a matrix.
  if( ( is.vector( x ) | is.matrix( x ) ) == FALSE ){ stop( "x must be a vector or a matrix." ) }
  if( length( l ) != length( u ) ){ stop( "The length of l must be equal to the length of u." ) }

  # Number of dimensions.
  p <- length( l )

  # First if x is a vector.
  if( is.vector( x ) ){
    if( length( x ) == p ){
      x <- matrix( x, nrow = 1 )
    }else{
      if( p == 1 ){
        x <- matrix( x, ncol = 1 )
      }else{
        stop( "If x is a vector, then it must either be of the lengths of l and u, or the lengths of l and u must be equal to 1.")
      }
    }
  }else{
    if( ncol( x ) != p ){ stop( "If x is a matrix, it must have the number of columns equal to the lengths of l and u." ) }
  }

  # Calculate a matrix of zeroes and ones depending on whether each point lies in the corresponding range of each dimension of the hypercube.
  zero_one_dens_matrix <- ( ( t( x ) < u ) & ( t( x ) > l ) )

  # Find out which points are in the hypercube by summing over the columns of the above matrix, and divide by the product of the hypercube dimension ranges to replace the TRUEs with the correct density.
  d <- ( colSums( zero_one_dens_matrix ) == p ) / prod( u - l )

  # Return the object.
  return( d )

}
