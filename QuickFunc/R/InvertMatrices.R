#' Invert Each Matrix in an Array
#'
#' @description Invert each of the matrices in an array and return an array of inverted matrices.  Note that if some of the matrices can't be inverted then a matrix of "NA"'s is put in it's place.
#'
#' @param V a 3-dimensional array
#'
#' @return an array, each matrix of which is the inversion of the matrices in V.
#' @export
#'
#' @examples
#' V <- array( stats::rnorm( 18, 0, 0.1) + rep( c( diag( 3 ) ), 2 ), dim = c(3,3,2) )
#' # ensure that in this example both matrices will be invertible
#' InvertMatrices( V )
InvertMatrices <- function( V ){

  # check the inputs
  if( ( is.array( V ) | length( dim( V ) ) == 3 ) == FALSE ){ stop( "V must be an array of dimension 3." ) }
  if( dim( V )[1] != dim( V )[2] ){ stop( "The first and second dimension of V must be equal, else we do not have square matrices to invert." ) }

  # An array of NAs the same size as V.
  V_inverse <- array( NA, dim = dim( V ) )

  # For each level of the 3rd dimension, try to invert the corresponding matrix in V and slot it in.
  for( i in 1:( dim( V_inverse )[3] ) ){

    try( V_inverse[,,i] <- solve( V[,,i] ) )

  }

  # Record which levels of the array V_inverse are matrices of NAs due to the corresponding matrix in V being singular/non-invertible.
  which_NAs <- which( is.na( V_inverse[1,1,] ) )

  # If there are some NAs...
  if( sum( which_NAs ) != 0 ){

    # ...write out a warning for each matrix that could not be inverted.
    for( j in 1:length( which_NAs ) ){ warning( paste( "matrix", which_NAs[j], "inverted unsuccessfully", sep=" ") ) }

  }

  # Return the object.
  return( V_inverse )

}
