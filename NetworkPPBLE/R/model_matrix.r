#' Construct model matrix for any dataframe and model.
#'
#' @param x set of training points in the input space, given as the rows of a matrix or dataframe.
#' @param model model used to define the basis functions of the regression polynomial.  If given as \code{1} (default) the regression matrix G is taken to be (1,x).  If given as \code{2} or \code{3}, the model matrix \code{MM} is taken to include the full quadratic or cubic terms (respectively) of the quantities in x.  Otherwise a model can be specified from which the regression matrix G can be calculated.
#'
#' @return Model matrix.
#' @export
#'
#' @examples
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' model_matrix( x )
#' model_matrix( x, 2 )
#'
model_matrix <- function( x, model = 1 ){

  # Check input conditions.
  if( ( is.matrix( x ) | is.data.frame ( x ) ) == FALSE ){ stop( "x should be a matrix or a dataframe." ) }

  # If mean_function_model is given as 1, just take model matrix to be x with a vector of 1's as first column.
  if( identical( model, 1 ) ){

    # Calculate the number of rows of x.
    n <- NROW( x )

    # Calculate the Model Matrix (MM) - in this case it is just x with a vector of 1's attached as the first column.
    MM <- as.matrix( cbind( rep( 1, n ), x ) )

  }else{

    # else if mean_function_model is another integer, construct the mean_function_model as the full linear model of order that integer.

    if( identical( typeof( model ) , "double" ) ){

      model <- QuickFunc::FullLinearModel( X = data.frame( x ), order = model )

    }

    # Now G is just the model matrix of mean_function_model.
    MM <- QuickFunc::ModelMatrix( X = data.frame( x ), model = model )

  }

  # Return the model matrix.
  return( MM )

}
