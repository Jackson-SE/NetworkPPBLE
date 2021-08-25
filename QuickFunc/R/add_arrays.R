#' @title Add Arrays
#'
#' @description Add arrays of different sizes across chosen dimensions of the larger array.
#'
#' @param A An array.
#' @param B An array or vector.
#' @param d A vector, indicating the dimensions of A over which we want to add B.
#' @param operation The operation to be applied - by default addition, but multiplication is also available.
#'
#' @return The array resulting from adding array \code{B} to each layer of array \code{A} across specified dimensions \code{d}.
#' @export
#'
#' @examples
#' A <- array( 1:24, dim = c(2,3,4) )
#' B <- matrix( 1:12, nrow = 3 )
#' add_arrays( A, B, d = 1 )
#' add_arrays( A, B, d = 1, operation = "multiplication" )
#'
add_arrays <- function( A, B, d, operation = "addition" ){

  # Dimension of each vector/array.
  ndA <- length( dim( A ) )
  if( is.vector( B ) ){
    ndB <- 1
    dB <- length( B )
  }else{
    ndB <- length( dim( B ) )
    dB <- dim( B )
  }

  # Check input conditions.
  if( ( is.array( B ) | is.vector( B ) ) == FALSE ){ stop( "B must be a vector or an array." ) }
  if( is.array( A ) == FALSE ){ stop( "A must be an array (of dimension greater than the dimension of B)." ) }
  if( sum( d %% 1 ) != 0 | length( d ) > ndA | max( d ) > ndA ){ stop( "d must be a vector of integers with values less than or equal to the dimension of A" ) }
  if( ndB >= ndA ){ stop( "The dimension of B must be smaller than the dimension of A" ) }
  if( identical( dim(A)[-d], dB ) == FALSE ){ stop( "The size of B does not match up with the layers of A across dimensions d.") }

  if( identical( operation, "addition" ) ){

    # Apply the addition.
    W <- apply( A, d, function( S ){ S + B } )

  }

  if( identical( operation, "multiplication" ) ){

    # Apply the addition.
    W <- apply( A, d, function( S ){ S * B } )

  }

  # Convert the result to an array.
  V <- array( W, dim = c( dB, dim(A)[d] ) )

  # Permute the dimensions of the array so that they are in the correct order.
  permutation <- rep( NA, ndA )
  permutation[-d] <- 1:ndB
  permutation[d] <- ( ndB + 1 ):ndA

  U <- aperm( V, permutation )

  # Return object.
  return( U )

}
