#' Compare and Add Variance Arrays and Vectors
#'
#' @description Check whether the dimension (up to 3) of two objects are the same - then perform appropriate addition of the two.
#'
#' If one is an array and the other a matrix, the matrix is added to each layer of the array.  Otherwise, addition happens as if the "+" operator had been used between the two.
#'
#' This function needs amending when we recall what the intention of said function was (adding variance arrays/matrices).  Think it may be good if there are more checks involved.  Also, do the first two dimensions (of an array) need to be equal?   If the answer if yes, then the example needs changing and this needs to be checked for.
#'
#' @param A array (of dimension 2 or 3) or vector.
#' @param B array (of dimension 2 or 3) or vector.
#'
#' @return Array resulting from the appropriate addition between A and B.
#' @export
#'
#' @examples
#' A <- array( 1:24, dim = c( 2,3,4 ) )
#' B <- matrix( 1:6, nrow=2 )
#' CompareAndAdd( A, B )
CompareAndAdd <- function(A, B){

  # Check the input conditions.
  if( ( is.array( A ) | is.vector( A )  ) == FALSE ){ stop( "A must be an array (of dimension 2 or 3) or vector." ) }
  if( ( is.array( B ) | is.vector( B )  ) == FALSE ){ stop( "B must be an array (of dimension 2 or 3) or vector." ) }

  if( length( dim( A ) ) == 3 & length( dim( B ) ) == 2 ){

    W <- QuickFunc::add_arrays( A, B, d = 3 )

  }else{

    if( length( dim( A ) ) == 2 & length( dim( B ) ) == 3 ){

      W <- QuickFunc::add_arrays( B, A, d = 3 )

    }else{

      W <- A + B

    }

  }

  return(W)

}
