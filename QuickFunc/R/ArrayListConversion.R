#' Array List Conversion
#'
#' @description Convert lists of vectors or arrays (of dimension d-1) to an array of dimension d, with the elements of the list indicated by the value of the dth dimension.
#'
#' Also, convert a matrix to a list of vectors, either by row or column.
#'
#' @param X A list of vectors/matrices or 2/3 dimensional array to be converted.
#' @param rows Option only for converting a matrix to a list of vectors which specifies whether the vectors in the list
#' are the rows (\code{TRUE}) or columns (\code{FALSE}, default) of the matrix.
#' @param levels Option for converting to a list - the names of the items of the list to have.
#'
#' @return A 2/3 dimensional array or list, depending on \code{X}.
#' @export
#'
#' @examples
#' X <- matrix( 1:12, ncol = 3 )
#' ArrayListConversion( X, levels = c("cat", "dog", "rabbit") )
#' Y <- matrix( 1:6, ncol = 2)
#' Z <- matrix( 7:12, ncol = 2)
#' L <- list( "cat" = Y, "dog" = Z )
#' ArrayListConversion( L )
ArrayListConversion <- function(X, rows=FALSE, levels = NULL){

  # Check the input conditions.
  if( ( is.matrix( X ) | is.list( X ) ) == FALSE ){ stop( "X must be a matrix or a list." ) }

  # If X is a matrix...
  if( is.matrix( X ) ){

    # If we want each column of X to become a vector in the list...
    if( rows == FALSE ){

      # Check to see if there are any prescibed names for the vectors, otherwise just number numerically.
      if( identical( levels, NULL ) ){ levels <- 1:ncol( X ) }

      # Convert X accordingly into a list of vectors.
      Y <- split( X, rep( levels, each = nrow( X ) ) )

    # Otherwise we want each row of X to become a vector in the list...
    }else{

      # Check to see if there are any prescibed names for the vectors, otherwise just number numerically.
      if( identical( levels, NULL ) ){ levels <- 1:nrow( X ) }

      # Convert X accordingly into a list of vectors.
      Y <- split( t( X ), rep( levels, each = ncol( X ) ) )

    }

  }

  # Otherwise X must be a list...
  if( is.list( X ) ){

    # The dimension of each element of X.
    if( is.array( X[[1]] ) ){ dX1 <- dim( X[[1]] ) }else{ dX1 <- length ( X[[1]] ) }

    # Form an array from the elements of X, where each element of the list is indicated by the value in the dth dimension.
    Y <- array( as.numeric( unlist( X ) ), dim = c( dX1, length( X ) ) )

  }

  # Return the object.
  return(Y)

}
