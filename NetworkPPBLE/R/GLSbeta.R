#' Generalised Least Squares Estimate for Beta
#'
#' @param fx a set of n model runs, each with a k-component model output, given as an n by k matrix or n-vector.
#' @param C correlation matrix between the model runs \code{fx}.
#' @param G model matrix for the set of runs \code{fx}.
#'
#' @return
#' \item{betahatGLS}{Generalised Least Squares estimate for beta coefficients}
#' \item{Q}{Q matrix - corresponding to G^TC^{-1}G}
#' @export
#'
#' @examples
#' f <- function( x ){ x[2] * sin( x[1] ) + x[1] * cos( x[2] ) }
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' fx <- apply( x, 1, f )
#' C <- GaussianCF( x, theta = c(0.4, 0.6) )
#' G <- cbind( rep( 1, 10 ), x )
#' GLSbeta( fx, C, G )
GLSbeta <- function( fx, C, G ){

  # Check input conditions.
  if( ( is.vector( fx ) | is.matrix( fx ) ) == FALSE ){ stop( "fx must be a vector or a matrix" ) }
  if( ( is.matrix( C ) & nrow( C ) == ncol( C ) ) == FALSE ){ stop( "C must be a square (correlation) matrix" ) }
  if( is.matrix( G ) == FALSE ){ stop( "G must be a (model) matrix" ) }
  if( nrow( C ) != nrow( G ) ){ stop( "G must have the same number of rows as the number
    of rows and columns in C." ) }
  if( NROW( fx ) != nrow( C ) ){ stop( "fx must be the same length as the number of rows and columns in C" ) }

  # C^(-1)_G
  L <- t( chol( C ) )
  Cinv_G <- solve( t( L ), solve( L, G ) )

  # Q = G^T_C^(-1)_G
  Q <- t( G ) %*% Cinv_G

  # betaHatGLS
  K = t( chol( Q ) )
  betahatGLS <- solve( t( K ), solve( K, t( Cinv_G ) ) ) %*% fx

  return( list("betahatGLS" = betahatGLS, "Q" = Q) )

}
