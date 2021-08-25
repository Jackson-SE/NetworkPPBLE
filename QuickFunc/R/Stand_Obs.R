#' Standardised Observation
#'
#' @description Calculate the standardised observation of a set of quantities.
#'
#' @param x Observed value, given as a vector or array.
#' @param EX Expectation of X, given as a vector or array which is the same dimension of \code{x}.
#' @param VarX Variance of X, given as a vector or arraywhich is the same dimension of \code{x}.
#' @param abs Should the absolute standardised observation be calculated?
#'
#' @return the standardised observations (absolute if applicable) as a vector or array the same dimension as \code{x}.
#' @export
#'
#' @details standardised observation is calculated as \code{( x - EX ) / sqrt( VarX )}
#'
#' @examples
#' EX = c(3, 8, 2)
#' VarX = rep( 0.7, 3 )
#' x = c(3.82, 6.34, 1.89)
#' Stand_Obs( x, EX, VarX )
Stand_Obs <- function( x, EX, VarX, abs = TRUE ){

  # Check input conditions.
  if( ( is.array( x ) | is.vector( x ) ) == FALSE ){ stop( "x must be a vector or array." ) }
  if( ( is.array( EX ) | is.vector( EX ) ) == FALSE ){ stop( "EX must be a vector or array." ) }
  if( ( is.array( VarX ) | is.vector( VarX ) ) == FALSE ){ stop( "VarX must be a vector or array." ) }

  if( is.array( x ) ){
    if( ( identical( dim( x ), dim ( EX ) ) | identical( dim( x ), dim( VarX ) ) ) == FALSE ){ stop( "The dimensions of x, EX and VarX must be the same.") }
  }else{
    if( length( x ) != length( EX ) | length( x ) != length( VarX ) ){ stop( "The length of x, EX and VarX must be the same.") }
  }

  if (abs == TRUE){
    SO <- abs( x - EX ) / sqrt( VarX )
  }else{
    SO <- ( x - EX ) / sqrt( VarX )
  }

  return( SO )

}
