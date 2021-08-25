#' Gaussian Correlation Function with Uncertain Inputs
#'
#' @param EX a vector, matrix or dataframe of the expected values of X
#' @param EY a vector, matrix or dataframe of the expected values of Y
#' @param VarX a vector, matrix or dataframe of the variances of X
#' @param VarY a vector, matrix or dataframe of the variances of Y
#' @param CovXY a matrix or array of the covariances between the points X and Y.
#' @param theta a vector of correlation length parameter values (one for each column of EX).
#' @param delta an (optional) scalar nugget parameter.
#'
#' @return Gaussian correlation function value between X and Y,
#' given as a matrix of dimension \code{nrow(EX)} by \code{nrow(EY)}.
#' @export
#'
#' @examples
#' EX <- matrix( rnorm( 10 ), ncol = 2 )
#' EY <- matrix( runif( 6 ), ncol = 2 )
#' VarX <- matrix ( rep( 0.01, 10 ), ncol = 2 )
#' VarY <- matrix ( rep( 0.01, 6 ), ncol = 2 )
#' theta <- c( 0.5, 0.8 )
#' GaussianCFUI( EX, EY,  VarX, VarY, theta = theta )
#' GaussianCFUI( as.data.frame( EX ), EY, VarX, VarY, theta = theta )
GaussianCFUI <- function( EX, EY = EX, VarX = 0, VarY = 0, CovXY = 0, theta, delta = 0 ){

  # If EX and EY are vectors, then we turn them into a matrix with a single column.
  if( is.vector( EX ) == TRUE ){
    EX <- matrix( EX, ncol = 1 )
  }
  if( is.vector( EY ) == TRUE ){
    EY <- matrix( EY, ncol = 1 )
  }

  # Check input conditions.
  if( ( is.matrix( EX ) | is.data.frame( EX ) ) == FALSE ){ stop( "EX must be a vector, matrix or dataframe." ) }
  if( ( is.matrix( EY ) | is.data.frame( EY ) ) == FALSE ){ stop( "EY must be a vector, matrix or dataframe." ) }
  if( ncol( EX ) != ncol( EY ) ){ stop( "EX and EY must have the same number of columns." ) }
  if( length( theta ) != ncol( EX ) ){ stop( "theta must be the same length as the number of columns of X." ) }
  if( ( is.vector( delta ) & length( delta ) == 1 & delta >= 0 ) == FALSE ){ stop( "delta must be a non-negative scalar" ) }

  # Convert a dataframe to a matrix if necessary.
  if( is.data.frame( EX ) ){ EX <- as.matrix( EX ) }
  if( is.data.frame( EY ) ){ EY <- as.matrix( EY ) }
  if( is.data.frame( VarX ) ){ VarX <- as.matrix( VarX ) }
  if( is.data.frame( VarY ) ){ VarY <- as.matrix( VarY ) }

  # If VarX, VarY and/or CovXY is zero, then make them matrices/arrays of the required dimensions:
  if( identical( VarX, 0 ) ){
    VarX <- 0 * EX
  }
  if( identical( VarY, 0 ) ){
    VarY <- 0 * EY
  }

  # If VarX, VarY are vectors, then we turn them into a matrix with a single column.
  if( is.vector( VarX ) == TRUE ){
    VarX <- matrix( VarX, ncol = 1 )
  }
  if( is.vector( VarY ) == TRUE ){
    VarY <- matrix( VarY, ncol = 1 )
  }

  # If CovXY is a matrix, then we turn it into an array with a third dimension size of 1.
  if( is.matrix( CovXY ) == TRUE ){
    CovXY <- array( CovXY, dim = c(nrow( CovXY ), ncol( CovXY ), 1 ) )
  }


  ## Compute the Gaussian correlation between each row of X and each row of Y.

  # Calculate the distance between EX and EY, first checking whether X is identical to Y.
  # If so, then use function dist instead of pdist to avoid R returning an error.
  if( identical( EX, EY ) ){
    squared_dist_EXEY <- as.matrix( stats::dist( QuickFunc::AVEM( EX, 1 / theta ) ) )^2
  }else{
    squared_dist_EXEY <- as.matrix( pdist::pdist( QuickFunc::AVEM( EX, 1 / theta ), QuickFunc::AVEM( EY, 1 / theta ) ) )^2
  }

  SumVarX <- rowSums( QuickFunc::AVEM( VarX, 1 / theta^2 ) )
  SumVarY <- rowSums( QuickFunc::AVEM( VarY, 1 / theta^2 ) )

  # If CovXY is 0, then the SumCovXY can be taken as 0, otherwise we apply the calculation below.
  if( identical( CovXY, 0 ) ){
    SumCovXY <- 0
  }else{
    SumCovXY <- apply( QuickFunc::AVEM( CovXY, 1/theta^2 ), 1:2, sum )
  }

  # Calculate the sum of VarX and VarY for each pair of points.
  SumVarXVarY <- kronecker( SumVarX, t( SumVarY ), "+" )

  # The correlation between X and Y is given as follows.
  cXY <- (1 - delta) * exp( - ( squared_dist_EXEY + SumVarXVarY - 2 * SumCovXY ) )

  # Return the result.
  return( cXY )

}
