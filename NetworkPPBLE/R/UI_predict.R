#' PPBLE predict with Uncertain Inputs
#'
#' @description predict the value of a model at some new input locations, along with a measure of uncertainty, using a PPBLE of class \code{emulate}.  We assume that the points at which we wish to predict are uncertain, given by an expected value and a variance (Bayes linear).  Having said this, we assume that the emulaor training points were fixed and known, hence we can use a regular object of class \code{emulate}.
#'
#' @param object an object of class \code{emulate}
#' @param EX vector or matrix of expected values.
#' @param VarX vector or matrix of variances.
#' @param UICF an Uncertain Inputs correlation function to be used.  It is assumed that this is an extension of \code{CF} used by \code{\link[NetworkPPBLE]{emulate}} when the emulator was built.  As such, it assumes that any additional parameters are the same. It also assumes that the first four objects of this correlation function are the expectations followed by the variances of the two matrices of points at which to run the correlation function.
#' @param batch_size the size of the batches at which to perform the predictions, default is 100.
#'
#' @return
#' \item{EfX}{BL adjusted expectation for f(X)}
#' \item{VarfX}{BL adjusted variance for f(X)}
#' @export
#'
#' @examples
#' f <- function( x ){ c( x[2] * sin( x[1] ) + x[1] * cos( x[2] ), 2 * x[1] + 3 * x[2] / x[1] ) }
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' fx <- t( apply( x, 1, f ) )
#' theta <- c( 0.4, 0.6 )
#' emulator <- emulate( x = x, fx = fx, CF = GaussianCF,
#'                   CF_para = list( theta = theta, delta = 0.0001 ) )
#' E_Y <- matrix( runif( 16, 0.2, 1.2), ncol = 2 )
#' Var_Y <- matrix( rep( 0.01, 16 ), ncol = 2 )
#' UI_predict( emulator, E_Y, Var_Y, UICF = GaussianCFUI )
UI_predict <- function( object, EX, VarX, UICF, batch_size = 100 ){

  # Check input conditions.
  if( is.na( match( "emulate", class( object ) ) ) ){ stop( "object is expected to be of class emulate." ) }
  if( identical( NROW( EX ), NROW( VarX ) ) == FALSE | identical( NCOL( EX ), NCOL( VarX ) ) == FALSE ){
    stop( "The dimension of Efx and Varfx should be the same." ) }
  if( NCOL( EX ) != NCOL( object$xv ) ){ stop( "The number of columns of EX and VarX should be the same as that of the emulator training points xv." ) }
  if( identical( object$mean_function_model, 1 ) == FALSE ){ stop( "mean_function_model must be 1 for UI to proceed." ) }

  # Extract names out of the "emulate" object.
  # QuickFunc::ExtNames( object, pos = 1 )
  #
  # # Rename the variables out of the object environment.
  xv <- object$xv
  fxv <- object$fxv
  betahatGLS <- object$betahatGLS
  Qinv <- object$Qinv
  s2 <- object$s2
  mean_function_model <- object$mean_function_model
  Cinv <- object$Cinv
  G <- object$G
  res <- object$res
  CF <- object$CF
  CF_para <- object$CF_para

  # Number of prediction points.
  n <- NROW( EX )

  # Number of regression components.
  p <- NCOL( EX )

  # Number of output components.
  k <- NCOL( fxv )

  # Number of training points.
  nv <- nrow( xv )

  # g(x) function for test points - assumed to be EX with intercept. Note that this needs expanding (see previous code not in package) if the uncertain inputs have a correlated correlation structure between them.
  EgX <- cbind( rep( 1, n ), EX )
  VargX <- cbind( rep( 0, n ), VarX )

  # predict the regression model's output for each point in xu.
  EgX_EFbeta <- EgX %*% betahatGLS

  # Set up matrices for the emulator estimation and uncertainty.
  E_uX <- matrix( NA, nrow = n , ncol = k )
  Var_uX <- Cov_gXbeta_uX <- rep( NA, n )

  # Split the EX, VarX matrices up into blocks of "batch_size" (default 100) points for much more manageable emulation.
  for( i in 1:ceiling( n / batch_size ) ){

    if( i * batch_size >= n ){

      EX_batch <- EX[( (i-1) * batch_size + 1):n,,drop=FALSE]
      VarX_batch <- VarX[( (i-1) * batch_size + 1):n,,drop=FALSE]
      X_batch_indices <- ( (i-1) * batch_size + 1 ):n

    }else{

      EX_batch <- EX[ ( (i-1) * batch_size + 1 ):( i * batch_size ),,drop=FALSE]
      VarX_batch <- VarX[ ( (i-1) * batch_size + 1 ):( i * batch_size ),,drop=FALSE]
      X_batch_indices <- ( ( i-1 ) * batch_size + 1 ):( i * batch_size )

    }

    # Correlation matrix between current x batch sub-matrix and xv training matrix.
    formals( UICF )[1:4] <- list( EX_batch, xv, VarX_batch, 0 )
    cX <- do.call( UICF, CF_para )

    # Useful Calculations
    cX_Cinv <- cX %*% Cinv
    cX_Cinv_G <- cX_Cinv %*% G
    Qinv_G_Cinv_cX <- Qinv %*% t( cX_Cinv_G )

    # Equation for E_ux.
    E_uX[X_batch_indices,] <- cX_Cinv %*% res

    # Finding the diagonal elements only of the matrix Var_ux.
    Var_uX[X_batch_indices] <- 1 - rowSums( cX_Cinv * cX ) + colSums( t( cX_Cinv_G ) * Qinv_G_Cinv_cX )

    # Cov[gxbeta, ux] vector.
    Cov_gXbeta_uX[X_batch_indices] <- colSums( t( EgX[X_batch_indices,,drop=FALSE] ) * Qinv_G_Cinv_cX )

  }

  # trace( Q %*% E[g(X)g(X)] ) - this is an n-vector
  trace_VarFbeta_EgXgX <- rowSums( t( diag( Qinv ) * t( VargX ) ) - EgX * ( EgX %*% Qinv ) )

  # diag( EF_beta %*% Var_gX %*% EF_beta ) - diagonal elements only.
  diag_EFbeta_VargX_EFbeta <- VargX %*% betahatGLS^2

  # Adjusted expectation for each element of x.
  EfX <- EgX_EFbeta + E_uX

  # Adjusted variance for each element of x.
  VarfX <- ( trace_VarFbeta_EgXgX + Var_uX - 2 * Cov_gXbeta_uX ) %*% t( s2 ) + diag_EFbeta_VargX_EFbeta

  # Return the result.
  return( list( "EfX" = EfX, "VarfX" = VarfX ) )

}
