#' Full Linear Model Generation
#'
#' @description Generate the full first, second or third order linear model for a given dataframe, X.
#' Whilst the actual models can be generated in this way, this function is mostly a useful way for extracting model formulae.
#'
#' @param X dataframe (not a matrix).
#' @param y (optional) vector of responses
#' @param order order of the polynomial fit
#'
#' @return Full linear model for y given X.
#' @export
#'
#' @examples
#' data( USArrests )
#' FullLinearModel( USArrests )
#' FullLinearModel( USArrests, order = 2 )
#' FullLinearModel( USArrests, order = 3 )
#' FullLinearModel( USArrests[,c("UrbanPop", "Assault")], y = USArrests[,"Murder"], order = 2 )
FullLinearModel <- function( X, y = rep( 1, nrow( X ) ), order = 1 ){

  # Check input conditions.
  if( sum( 1:3 == order ) != 1 ){ stop( "order must be 1, 2 or 3" ) }
  if( is.data.frame( X ) == FALSE ){ stop( "X must be a dataframe" ) }
  if( length( y ) != nrow( X ) ){ stop( "length of y must be equal to the number of rows in X" ) }

  # First-order linear model.
  FLmod <- stats::lm( y ~ ., data = X )

  # If we only want a first-order model, we return this object.
  if( identical( order, 1 ) ){

    return(FLmod)

  # Otherwise we continue and add the second order terms.
  }else{

    variable_names <- names( FLmod$coefficients )[-1]

    FO <- paste( variable_names, collapse="+" )

    SO <- paste( "(", FO, "):(", FO, ")", "+", paste( "I(", variable_names, "^2)", collapse="+", sep="" ) )

    # If we want a second order model, we now construct  the full quadratic model using the terms above.
    if( identical( order, 2 ) ){

      form1 <- stats::as.formula( paste( "y~", SO ) )

      FQmod <- stats::lm( form1, data = X )

      return( FQmod )

    # Otherwise we add the third order terms and construct the full cubic model.
    }else{

      TO <- paste( "(", SO, "):(", FO, ")", "+", paste( "I(", variable_names, "^2)", collapse="+", sep="" ) )

      form1 <- stats::as.formula( paste( "y~", TO ) )

      FCmod <- stats::lm( form1, data = X )

      return( FCmod )

    }

  }

}
