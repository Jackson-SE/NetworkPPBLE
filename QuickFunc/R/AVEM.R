#' @title Array Vector-Element Multiplication
#'
#' @description Multiply each matrix/column in a 3D/2D array by the corresponding element of a vector.
#'
#' @param A two or three-dimensional array.
#' @param v vector
#'
#' @return array resulting from multiplying the matrices/columns in A by the corresponding elements in v.
#' @export
#'
#' @examples
#' A <- array( 1:24, dim = c( 2,3,4 ) )
#' b = 1:4
#' AVEM( A, b )
AVEM <- function( A, v ){

  # Check input conditions.
  if( ( is.array( A ) & sum( length( dim( A ) ) == 2:3 ) == 1 ) == FALSE ){ stop( "A must be a two- or three-dimensional array." ) }
  if( ( is.vector( v ) ) == FALSE ){ stop( "v must be a vector." ) }

  # Multiply each matrix or column of A by the corresponding element in v.
  if( length( dim( A ) ) == 3 ){
    B <- A * rep( v, times = rep( dim( A )[1] * dim( A )[2], dim( A )[3] ) )
  }else{
    B <- A * rep( v, times = rep( nrow( A ), ncol( A ) ) )
  }

  # Return the object.
  return(B)

}
