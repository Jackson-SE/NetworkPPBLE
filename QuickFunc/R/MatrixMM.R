#' Matrix MM
#'
#' @description Take the maximum or minimum of each column or row of a matrix.
#'
#' @param X a matrix or a dataframe.
#' @param maximum Take the maximum (\code{TRUE}) or minimum (\code{FALSE})...
#' @param column ...of each column (\code{TRUE}) or row (\code{FALSE})
#'
#' @return vector of maximum or minimum values of the columns or rows of X.
#' @export
#'
#' @examples
#' X <- matrix( runif( 6 ), ncol = 2 )
#' MatrixMM( X )
#' Y <- data.frame( X, row.names = c( "cat", "dog", "rabbit" ) )
#' MatrixMM( Y )
MatrixMM <- function( X, maximum = TRUE, column = TRUE ){

  # Check the input conditions.
  if( ( is.matrix( X ) | is.data.frame( X ) ) == FALSE ){ stop( "X must be a matrix or a dataframe." ) }

  if( maximum == TRUE & column == TRUE ){

    # Maximum of each column of X.
    Y <- do.call( pmax, as.data.frame( t( X ) ) )

  }

  if( maximum == FALSE & column == TRUE ){

    # Minimum of each column of X.
    Y <- do.call( pmin, as.data.frame( t( X ) ) )

  }

  if( maximum == TRUE & column == FALSE ){

    # Maximum of each row of X.
    Y <- do.call( pmax, as.data.frame( X ) )

  }

  if( maximum == FALSE & column == FALSE ){

    # Minimum of each row of X.
    Y <- do.call( pmin, as.data.frame( X ) )

  }

  return(Y)

}
