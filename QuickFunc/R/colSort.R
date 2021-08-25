#' Sort Each Column of Matrix
#'
#' @param X a matrix.
#' @param decreasing logical.  Should the sort order be increasing or decreasing?
#' @inheritParams base::order
#'
#' @return a matrix with each column containing the sorted elements of the columns in X.
#' @export
#'
#' @seealso
#' \code{\link[base]{order}}
#'
#' @examples
#' X <- matrix( sample( 50, 20 ), ncol = 4 )
#' colSort( X )
#' colSort( X, decreasing = TRUE )
colSort <- function( X, decreasing = FALSE, na.last = TRUE ){

  # Check the input conditions.
  if( ( is.matrix( X ) ) == FALSE ){ stop( "X must be a matrix." ) }

  if( identical( decreasing, FALSE ) ){

    Xsort <- matrix( X[order( col( X ), X, decreasing = FALSE, na.last = na.last )], ncol = ncol( X ) )

  }else{

    Xsort <- matrix( X[order( col( X ), X, decreasing = TRUE, na.last = na.last )], ncol = ncol( X ) )[,ncol(X):1]

  }

  # Return the result.
  return( Xsort )

}
