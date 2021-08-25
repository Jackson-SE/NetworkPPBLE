#' @title Show first rows and columns of a matrix or dataframe.
#'
#' @param X matrix or dataframe.
#' @param k scalar - number of rows to show.
#' @param l scalar - number of columns to show.
#'
#' @return first \code{k} rows and \code{l} columns of \code{X}
#' @export
#'
#' @examples
#' X <- matrix( 1:210, ncol = 15)
#' ab(X)
ab <- function( X, k = min( nrow( X ), ncol( X ), 10 ), l = k ){

  # Check the input conditions.
  if( ( is.matrix( X ) | is.data.frame( X ) ) == FALSE ){ stop( "X must be a matrix or a dataframe." ) }
  if( k %% 1 != 0 | k > nrow( X ) ){ stop( "k must be an integer less than or equal to the number of rows in X.") }
  if( l %% 1 != 0 | l > ncol( X ) ){ stop( "l must be an integer less than or equal to the number of rows in X." ) }

  X[1:k, 1:l]

}
