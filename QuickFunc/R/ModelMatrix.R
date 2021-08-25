#' Model Matrix
#'
#' @description Obtain the model matrix for a given data frame and model
#'
#' @param X dataframe (not a matrix)
#' @param model a model
#'
#' @return the corresponding model matrix
#' @export
#'
#' @examples
#' data(USArrests)
#' mod <- QuickFunc::FullLinearModel( USArrests[1:40,], order = 2 )
#' MM <- ModelMatrix( USArrests[1:10,], mod )
#' MM # Can use it for part of the training set.
#' MN <- ModelMatrix( USArrests[41:50,], mod )
#' MN # Can use it for some prediction points.
ModelMatrix <- function( X, model ){

  # Check input conditions.
  if( is.data.frame( X ) == FALSE ){ stop( "X must be a dataframe." ) }
  if(  identical( setdiff( all.vars( stats::as.formula( model ) )[-1], dimnames( X )[[2]] ), character(0) ) == FALSE ){
    stop( "All of the (names of the) variables in the model must be contained as variables in X." )
  }

  # a dummy response vector y.
  y <- rep( 0, nrow( X ) )

  # calculate the model matrix from the given model.
  H <- stats::model.matrix( stats::as.formula( model ), data = cbind( y, X ) )

  return(H)

}
