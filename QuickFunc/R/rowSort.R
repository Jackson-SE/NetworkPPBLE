#' Sort Each Row of Matrix
#'
#' @param X a matrix.
#' @param decreasing logical (defaulting to \code{FALSE}).  Should the sort order be increasing or decreasing?
#' @inheritParams base::order
#'
#' @return a matrix with each row containing the sorted elements of the rows in X.
#' @export
#'
#' @seealso
#' \code{\link[base]{order}}
#'
#' @examples
#' X <- matrix( sample( 50, 20 ), ncol = 4 )
#' rowSort( X )
#' rowSort( X, decreasing = TRUE )
rowSort <- function( X, decreasing = FALSE, na.last = TRUE ){

  # Check the input conditions.
  if( ( is.matrix( X ) ) == FALSE ){ stop( "X must be a matrix." ) }

  if( identical( decreasing, FALSE ) ){

    Xsort <- matrix( X[order( row( X ), X, decreasing = FALSE, na.last = na.last )], ncol = ncol( X ), byrow = TRUE )

  }else{

    Xsort <- matrix( X[order( row( X ), X, decreasing = TRUE, na.last = na.last )], ncol = ncol( X ), byrow = TRUE )[nrow(X):1,]

  }

  # Return the result.
  return( Xsort )

}
