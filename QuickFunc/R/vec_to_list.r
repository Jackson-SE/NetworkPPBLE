#' Convert a vector of elements into a list of vectors of prescribed lengths.
#'
#' @param z a vector.
#' @param lop a vector indicating how many elements of z should go into each vector in the list.
#' @param names optional names parameter for the elements of the constructed list.  If specified, it must be the same length as lop.
#'
#' @return a list of vectors, each taking several elements of z as presribed by the elements of lop.
#' @export
#'
#' @examples
#' z <- 1:10
#' vec_to_list( z )
#' vec_to_list( z, lop = c(2,3,5) )
#' vec_to_list( z, lop = c(2,3,5), names = c("dog", "cat", "rabbit") )
vec_to_list <- function( z,
                         lop = rep( 1, length( z ) ),
                         names = NA ){

  # Check input conditions.
  if( is.vector( z ) == FALSE ){ stop( "z must be a vector." ) }
  if( is.vector( lop ) == FALSE ){ stop( "lop must be a vector." ) }
  if( length( z ) != sum( lop ) ){ stop( "the sum of lop must be equal to the length of z." ) }
  if( ( identical( names, NA ) | length( names ) == length( lop ) ) == FALSE ){ stop( "names must either be NA or of length equal to that of lop." ) }

  # Start with an empty list.
  z_list <- list()

  # Set a counter to 0.
  counter <- 0

  for( j in 1:length( lop ) ){

    # Put the next lop[j] elements of z as the jth object in z_list.
    z_list[[j]] <- z[(counter + 1):(counter + lop[j])]

    # Increase the counter.
    counter <- counter + lop[j]

  }

  # Assign the names if required.
  if( identical( names, NA ) == FALSE ){ names( z_list ) <- names }

  # Return the list.
  return( z_list )

}
