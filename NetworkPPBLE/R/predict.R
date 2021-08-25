#' PPBLE predict (emulate)
#'
#' @description predict the value of a model at some new input locations, along with a measure of uncertainty, using a PPBLE of class \code{emulate}.
#'
#' @param object an object of class \code{emulate}
#' @param x a set of training runs at which to make predictions using the \code{emulate} object, given as a vector, matrix or dataframe (the same type as xv)
#' @param batch_size the size of the batches at which to perform the predictions, default is 100.
#' @param ... additional arguments (although not quite sure what these may be)
#'
#' @return
#' \item{Efx}{BL adjusted expectation for f(x)}
#' \item{Varfx}{BL adjusted variance for f(x)}
#' @export
#'
#' @examples
#' f <- function(x){ x[2] * sin( x[1] ) + x[1] * cos( x[2] ) }
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' fx <- apply( x, 1, f )
#' theta <- c( 0.4, 0.6 )
#' emulator <- emulate( x = x, fx = fx, CF = GaussianCF,
#'             CF_para = list( theta = theta, delta = 0.0001 )  )
#' y <- matrix( runif( 16, 0.2, 1.2 ), ncol = 2 )
#' predict( emulator, y )
predict.emulate <- function( object, x = "training", batch_size = 100, ... ){

  # Check input conditions.
  if( is.na( match( "emulate", class( object ) ) ) ){ stop( "object is expected to be of class emulate." ) }

  # Extract names out of the "emulate" object.
  # QuickFunc::ExtNames( object )

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

  # If no alternative x is specified, then we predict at the training points xv.
  if( identical( x, "training" ) ){ x <- xv }

  # Check more input conditions.
  if( NCOL( x ) != NCOL( xv ) ){ stop( "x should have the same number of columns as the training runs object$xv." ) }
  if( identical( is.data.frame( xv ), is.data.frame( x ) ) == FALSE ){ stop( "x and xv should either both be vectors/matrices or both be dataframes." ) }

  # Number of points at which we want to predict.
  n <- NROW( x )

  # Number of output components.
  k <- NCOL( fxv )

  # g(x) function for test points.
  gx <- NetworkPPBLE::model_matrix( x = x, model = mean_function_model )

  # predict the regression model's output for each point in xu.
  gx_EF_beta <- gx %*% betahatGLS

  # Set up matrices for the emulator estimation and uncertainty.
  E_ux <- matrix( NA, nrow = n, ncol = k)
  Var_ux <- Cov_gxbeta_ux <- rep( NA, n )

  # Split the x matrix up into blocks of "batch_size" (default 100) points for much more manageable emulation.
  for( i in 1:ceiling( n / batch_size ) ){

    if( i * batch_size >= n ){

      x_batch <- x[ ( (i-1) * batch_size + 1 ):n,,drop=FALSE]
      x_batch_indices <- ( (i-1) * batch_size + 1 ):n

    }else{

      x_batch <- x[ ( (i-1) * batch_size + 1 ):( i * batch_size ),,drop=FALSE]
      x_batch_indices <- ( ( i-1 ) * batch_size + 1 ):( i * batch_size )

    }

    # Correlation matrix between current x batch sub-matrix and xv training matrix.
    formals( CF )[1:2] <- list( x_batch, xv )
    cx <- do.call( CF, CF_para )

    # Useful Calculations
    cx_Cinv <- cx %*% Cinv
    cx_Cinv_G <- cx_Cinv %*% G
    Qinv_G_Cinv_cx <- Qinv %*% t( cx_Cinv_G )

    # Equation for E_ux.
    E_ux[x_batch_indices,] <- cx_Cinv %*% res

    # Finding the diagonal elements only of the matrix Var_ux.
    Var_ux[x_batch_indices] <- 1 - rowSums( cx_Cinv * cx ) + colSums( t( cx_Cinv_G ) * Qinv_G_Cinv_cx )

    # Cov[gxbeta, ux] vector.
    Cov_gxbeta_ux[x_batch_indices] <- colSums( t( gx[x_batch_indices,,drop=FALSE] ) * Qinv_G_Cinv_cx )

  }

  # Adjusted expectation for each element of x.
  Efx <- gx_EF_beta + E_ux

  # Adjusted variance for each element of x.
  Varfx <- ( rowSums( ( gx %*% Qinv ) * gx ) + Var_ux - 2 * Cov_gxbeta_ux ) %*% t( s2 )

  return( list( "Efx" = Efx, "Varfx" = Varfx ) )

}
