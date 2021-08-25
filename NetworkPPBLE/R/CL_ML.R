#' Maximum likelihood for finding Correlation length parameters
#'
#' @param CF A function that can be used to calculate the correlation between two matrices or dataframes of points.
#' @param CF_para additional parameters to the function \code{CF}, along with their specified values, given as a list.
#' @param para_to_optim a vector of the names of the arguments to the function \code{CF} that which to assessed via maximum likelihood.
#' @param x a matrix of points (given by the rows)
#' @param fx output for each \code{x}.
#' @param G model matrix.  This can either be specifed here or the code can calculate this given \code{mean_function_model}.
#' @param mean_function_model this is a linear model that is used to calculate a model matrix \code{G}.
#' @param initial initial settings for the parameter values to be optimised.  These must either be given as a vector or a list of vectors, depending on how many arguments want to be optimised.
#' @param lower lower limits for the optimisation, given as a vector or list of vectors.
#' @param upper upper limits for the optimisation, given as a vector or list of vectors.
#' @param method method choice for optimisation.
#'
#' @return a list containing the found optimium correlation length parameter values.
#' @export
#'
#' @seealso
#' \code{\link[stats]{optim}}
#'
#' @examples
#' f <- function(x){ c( x[2] * sin( x[1] ) + x[1] * cos( x[2] ), 2 * x[1] + 3 * x[2] / x[1] ) }
#' x <- matrix( runif( 20, 0.2, 1.2 ), ncol = 2 )
#' fx <- t( apply( x, 1, f ) )
#' CL_ML( CF = NetworkPPBLE::GaussianCF,
#'        CF_para = list( delta = 0.0001 ),
#'        para_to_optim = "theta",
#'        x = x,
#'        fx = fx,
#'        mean_function_model = 1,
#'        initial = rep( 1, 2 )
#'         )
CL_ML <- function( CF = GaussianCF,
                   CF_para = list(),
                   para_to_optim,
                   x,
                   fx,
                   G = NA,
                   mean_function_model = NA,
                   initial,
                   lower = -Inf,
                   upper = Inf,
                   method = "Nelder-Mead" ){

  # Check input conditions.
  if( NROW ( x ) != NROW ( fx ) ){ stop( "The number of rows of x and fx must be the same." ) }
  if( identical( G, NA ) & identical( mean_function_model, NA ) ){ stop( "G and mean_function_model can't both be given as NAs." ) }

  # Check that initial is a vector or a list of vectors.  If initial is a list, find the length of each of its elements and store as a vector.
  if( is.list( initial ) ){
    lop <- rep( NA, length( initial ) )
    for( j in 1:length( initial ) ){
      if( is.vector( initial[[j]] ) == FALSE ){ stop( "initial must be a vector or a list of vectors." ) }
      lop[j] <- length( initial[[j]] )
    }
    init <- unlist( initial, use.names = FALSE )
  }else{
    if( is.vector( initial ) ){
      lop <- length( initial )
    }else{
      stop( "initial must be a vector or a list of vectors." )
    }
    init <- initial
  }

  # Lower and upper might be lists, then they need unlisting.
  if( is.list( lower ) ){ lower <- unlist( lower, use.names = FALSE ) }
  if( is.list( upper ) ){ upper <- unlist( upper, use.names = FALSE ) }

  # Optimisation function criteria.
  if( identical( lop, 1 ) ){
    if( identical( method, "Brent" ) == FALSE ){
      stop( "Method of optimisation in function optim for correlation lengths (ML) should be Brent if the optimisation is over a single dimension." )
    }
  }
  if( identical( method, "Brent" ) &
      ( identical( typeof( lower ), "double" ) == FALSE | identical( typeof( upper ), "double" ) == FALSE |
          identical( lower, -Inf ) | identical( upper, Inf ) ) ){
    stop( "If the method of optimisation is Brent, then a finite upper and lower value must be provided (take into account the log transformation)" )
  }
  if( identical( typeof( lower ), "double" ) & min( lower ) <= 0.0001 & identical( lower, -Inf ) == FALSE ){
    stop( "lower must be greater than 0.0001 as a result of the log transformation applied for optimisation.")
  }

  # Obtain the log initial value.
  log_init <- log( init - 0.0001 )

  # Obtain the log lower limit.
  if( identical( lower, -Inf ) ){
    loglower <- lower
  }else{
    loglower <- log( lower - 0.0001 )
  }

  # Obtain the log upper limit.
  if( identical( upper, Inf ) ){
    logupper <- upper
  }else{
    logupper <- log( upper - 0.0001 )
  }

  # Obtain G from mean_function_model if required.
  if( identical( G, NA ) ){

    # Obtain model matrix G from mean_function_model.
    G <- NetworkPPBLE::model_matrix( x = x, model = mean_function_model )

  }

  # number of training points
  n <- NROW( fx )

  # Number of output components.
  k <- NCOL( fx )

  # number of regression components.
  q <- ncol( G )

  # Number of variables.
  p <- NCOL( x )

  # Put the formals of the first two elements of CF as x.
  formals( CF )[1:2] <- list( x, x )

  # Define Log Likelihood function.
  LL <- function( logz ){

    # Turn logz into z.
    z <- exp( logz ) + 0.0001

    # z needs to be a list.
    z <- QuickFunc::vec_to_list( z, lop = lop )

    # Assign the value of z to para_to_optim.
    for( i in 1:length( para_to_optim ) ){ CF_para[[para_to_optim[i]]] <- z[[i]] }

    # Calculate the correlation matrix.
    C <- do.call( CF, CF_para )

    # Calculate betahatGLS as a function of theta, which features in the log likelihood expression for theta.
    betahatGLSestimation <- NetworkPPBLE::GLSbeta( fx = fx, C = C, G = G )
    betahatGLS <- betahatGLSestimation$betahatGLS

    # Q = t(G) %*% Cinv %*% G
    Q <- betahatGLSestimation$Q

    # G_beta
    G_beta <- G %*% betahatGLS

    # W = (Y - G_beta) Cinv (Y - G_beta)
    L <- t( chol( C ) )
    R <- solve( L ,  as.matrix( ( fx - G_beta ) ) )
    W <- colSums( R^2 )
    Wplus <- W[W>0]

    # Log likelihood expression:
    LL_z <- - (n - q) * sum( log( Wplus ) ) - k * as.numeric( determinant( C )$modulus ) - k * as.numeric( determinant( Q )$modulus )

    # Return log likelihood LL_z.
    return( LL_z )

  }

  # Carry out the optimisation for theta - note that the optimisation happens on a log scale.
  cl_optimisation <- stats::optim( log_init, LL, control = list( fnscale = -1 ), method = method, lower = loglower, upper = logupper )

  # Extract zhat - note the transformation back to the original scale.
  z_hat <- exp( cl_optimisation$par ) + 0.0001

  # z needs to be a list.
  z_hat <- QuickFunc::vec_to_list( z_hat, lop = lop )

  # Return both z_hat.
  return( z_hat )

}
