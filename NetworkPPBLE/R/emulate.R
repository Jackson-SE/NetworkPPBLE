#' PPBLE emulate
#'
#' @description generate an object of class \code{emulate} given a set of training points and the corresponding output of a model at those training points.
#'
#' @param x set of training points in the input space, given as the rows of a matrix or dataframe
#' @param fx model output corresponding to the training points in \code{x}, given as a vector or matrix.
#' @param mean_function_model model used to define the basis functions of the regression polynomial.  If given as \code{1} (default) the regression matrix G is taken to be (1,x).  If given as \code{2} or \code{3}, the regression matrix G is taken to include the full quadratic or cubic terms (respectively) of the quantities in x.  Otherwise a model can be specified from which the regression matrix G can be calculated.
#' @param s2 a vector of length equal to the number of columns of \code{fx} giving the scalar variance parameter for each output component.
#' @param CF a correlation function to be used by the emulator - it is assumed that the first two arguments of this function are to be objects (vectors, matrices or dataframes) for which the correlation function is to be computed.
#' @param CF_para specification of the value of any additional parameters that are required for the correlation function, given in the form of a list.  If a value is specified for a particular parameter, this value is used; if \code{NA} is specified, the function \code{CF_para_optim} will be used to optimise the values of those parameters; any additional parameters to \code{CF} not listed will take their default values as given by \code{CF} itself.  Note that any parameters that are not given default values by \code{CF} must be listed.
#' @param CF_para_optim a function that optimises some of the parameters of the function \code{CF}.
#' @param CF_para_optim_para a list of any parameters to the function \code{CF_para_optim} that require specifying, along with their desired specified values.  Any parameters of \code{CF_para_optim} not listed will take their default value (as given by \code{CF_para_optim}).  Note that any parameters that are not given default values by \code{CF_para_optim} must be listed.
#'
#' @return an object of class \code{emulate} which is a PPBLE that can be used to predict model output at further inputs, along with providing an uncertainty estimate.
#' @export
#'
#' @examples
#' f <- function(x){ c( x[2] * sin( x[1] ) + x[1] * cos( x[2] ), 2 * x[1] + 3 * x[2] / x[1] ) }
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' fx <- t( apply( x, 1, f ) )
#' theta <- c(0.4, 0.6)
#' emulator <- emulate( x = x, fx = fx, CF = GaussianCF,
#'                   CF_para = list( theta = theta, delta = 0.0001 ) )
#' emulator2 <- emulate( x = x, fx = fx[,1], CF_para = list( theta = theta, delta = 0.0001 ) )
#' X <- data.frame( x )
#' dimnames( X )[[2]] <- c( "X1", "X2" )
#' emulator3 <- emulate( x = X, fx = fx, CF = GaussianCF,
#'                    CF_para = list( theta = theta, delta = 0.0001 ) )
#' emulator4 <- emulate( x = x, fx = fx, CF = GaussianCF,
#'                       CF_para = list( theta = NA, delta = 0.0001 ),
#'                       CF_para_optim = CL_ML,
#'                       CF_para_optim_para = list( mean_function_model = 1,
#'                                                  initial = rep( 1, 2 ) ) )
emulate <- function( x,
                     fx,
                     mean_function_model = 1,
                     s2 = NA,
                     CF = NetworkPPBLE::GaussianCF,
                     CF_para = list(),
                     CF_para_optim = NULL,
                     CF_para_optim_para = list()
                     #  theta = NA, theta_initial = NA, theta_lower = -Inf, theta_upper = Inf, theta_method = "Nelder-Mead",
                     # E_beta = 0, Var_beta = "vague"
                     ){

  # Check input conditions.
  if( ( is.matrix( x ) | is.data.frame ( x ) ) == FALSE ){ stop( "x should be a matrix or a dataframe." ) }
  if( ( is.vector( fx ) | is.matrix ( fx ) ) == FALSE ){ stop( "fx should be a vector or a matrix." ) }
  if( NROW( x ) != NROW( fx ) ){ stop( "The number of rows of x should be equal to the number of rows of fx." ) }

  # Calculate the number of rows of x.
  n <- NROW( x )

  # Obtain model matrix G from mean_function_model.
  G <- NetworkPPBLE::model_matrix( x = x, model = mean_function_model )

  # Number of columns of G
  m <- NCOL( G )

  # Put the formals of the first two elements of CF as x.
  formals( CF )[1:2] <- list( x, x )

  # selection of the parameters theta and s2.  Note that whilst the correlation length parameters can be specified by ML or LOOCV (or be specified), s2 can only be estimated using ML (as LOOCV would be too computationally intensive as the cross-validation procedure would need to be performed for each grid point).

  # Find the names of which parameters to the acceptance probability generator function were defined as NA.  We are going to optimise these ones.
  para_to_optim <- names( which( is.na( CF_para ) ) )

  # If the length of the above list of names is not zero, then there is something to optimise!  And so...
  if( length( para_to_optim ) != 0 ){

    # Set the formals for the parameter optimisation function.
    formals( CF_para_optim )[1:3] <- list( CF, CF_para, para_to_optim )

    # Check if the formals of CF_para_optim are defined.
    for( i in 1:length( formals( CF_para_optim ) ) ){

      # This asks if both each formal of CF_para_optim has no default and is not provided in the CF_para_optim_para parameter list.
      if( identical( formals( CF_para_optim )[[i]], substitute() ) & ( names( formals( CF_para_optim ) )[[i]] %in% names( CF_para_optim_para ) ) == FALSE ){

        # If so, ask if the argument could also be given in the call of the outer function (in this case emulate itself).
        if( assertthat::has_args( f = NetworkPPBLE::emulate, args = names( formals( CF_para_optim ) )[[i]] ) ){

          # if it can, then we want to define it using this argument.
          formals( CF_para_optim )[[i]] <- get( names( formals( CF_para_optim ) )[[i]] )

        }else{
          # otherwise we need to return an error explaining the problem as the function cannot continue.
          stop( paste( "argument", names( formals( CF_para_optim ) )[[i]], "in CF_para_optim function not specified.", sep = " " ) )

        }

      }

    }

    # Calculate the optimal parameters using the parameter optimisation function.
    opt_par <- do.call( CF_para_optim, CF_para_optim_para )

    # Assign the value of z to para_to_optim.
    for( i in 1:length( para_to_optim ) ){ CF_para[[para_to_optim[i]]] <- opt_par[[i]] }

  }

  # Correlation matrix for the training points, and its inverse.
  C <- do.call( CF, CF_para )
  # L <- t( chol( C ) )
  # Cinv <- solve( t( L ), solve( L ) )
  Cinv <- solve( C )

  # betahatGLS
  GLSestimationBeta <- NetworkPPBLE::GLSbeta( fx = fx, C = C, G = G )
  betahatGLS <- GLSestimationBeta$betahatGLS

  # model residuals: res = F - G %*% E_F[beta]
  res <- fx - G %*% betahatGLS

  # if s2 is not NA, then it is a specified vector of values for s2 for each output component.
  if( identical( s2, NA ) == TRUE ){

    # s2_hat
    # R <- solve( L , res )
    # W <- colSums( R^2 )
    W <- colSums( t( t( res ) %*% Cinv ) * res )

    # Whether this is (n-p) or (n-p-2) depends on the likelihood estimation technique used.
    s2 <- W / ( n - m - 2 )

  }

  # Qinv
  Q <- GLSestimationBeta$Q
  # R <- t( chol( Q ) )
  # Qinv <- solve( t( R ), solve( R ) )
  Qinv <- solve( Q )

  # List all the necessary objects from this function in a list.
  CG <- list( "xv" = x, "fxv" = fx, "mean_function_model" = mean_function_model, "s2" = s2, "CF" = CF, "CF_para" = CF_para, "betahatGLS" = betahatGLS, "Qinv" = Qinv, "C" = C, "Cinv" = Cinv, "G" = G, "res" = res)

  # Append class type "emulate" to list object above.
  class( CG ) <- append( class( CG ), "emulate" )

  # Return the object.
  return( CG )

}
