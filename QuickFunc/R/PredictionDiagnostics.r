#' Prediction Diagnostics
#'
#' @description Obtain Mean Absolute Standardised Prediction Error (MASPE) and Root Mean Standardised Prediction Error (RMSPE) between a set of predictions \code{Efx, Varfx} and the true model or system value \code{fx}.
#'
#' @param fx Model/system value.
#' @param Efx Prediction.
#' @param Varfx Uncertainty variance.
#' @param var_nug optional variance nugget.
#'
#' @return MASPE and RMSPE
#' @export
#'
#' @examples
#' fx <- c(3,5,6)
#' Efx <- c(4,3,5.75)
#' Varfx <- rep( 0.8, 3 )
#' Prediction_diagnostics( fx, Efx, Varfx )
Prediction_diagnostics <- function ( fx, Efx, Varfx, var_nug = 0.0000000001 ){

  # Mean Absolute Standardised Prediction Error (MSPE)
  MASPE <- mean( abs( Efx - fx ) / sqrt( Varfx + var_nug ) )

  # Mean Squared Standardised Prediction Error (MSSPE)
  MSSPE <- mean( ( Efx - fx )^2 / ( Varfx + var_nug ) )

  # Root Mean Squared Prediction Error (RMSPE)
  RMSPE <- sqrt( mean( ( Efx - fx )^2 ) )

  # Return
  return( list( "MASPE" = MASPE, "MSSPE" = MSSPE, "RMSPE" = RMSPE ) )

}
