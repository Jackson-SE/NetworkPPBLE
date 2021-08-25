#' k Nearest Neighbour interpolation.
#'
#' @param x a vector or matrix of values for which we wish to interpolate some output given \code{y} and \code{z}.
#' @param y a vector or matrix of values for which we have the output value of interest (given by \code{z}).
#' @param z a vector or matrix of output values corresponding to the points in \code{y}.
#' @param k the number of nearest neighbours we wish to average over.
#' @param algorithm nearest neighbour searching algorithm.
#' @param p the power to which we wish to take the distance of each point to its nearest neighbours to in the inverse distance average weighting.
#'
#' @return a vector or matrix (depending on the whether \code{z} was a vector or matrix) of the k nearest neighbour interpolated predictions.
#' @export
#'
#' @details If \code{y} or \code{z} are vectors, it is assumed that the corresponding input or output dimension (respectively) is 1.  If \code{x} is a vector, then if the dimension of \code{y} is 1, it is assumed to be a vector of points at which to predict, otherwise it is assumed to be a single point (with dimension equal to that of \code{y}) at which to predict.
#'
#' @seealso
#' \code{\link[FNN]{get.knnx}}
#'
#' @examples
#' f <- function( y ){ c( sin( y[1] ) + cos( y[2] ), cos( y[1] ) + sin( y[2] ) ) }
#' y <- matrix( runif( 40 ), ncol = 2 )
#' z <- t( apply( y, 1, f ) )
#' x <- matrix( runif( 30 ), ncol = 2 )
#' kNNinterp( x, y, z )
#' kNNinterp( x, y, z[,1] )
#' kNNinterp( x[1,,drop=FALSE], y, z )
#' kNNinterp( x[1,], y, z )
kNNinterp <- function( x, y, z, k = 10, algorithm = "kd_tree", p = 2 ){

  # Check the input conditions.
  if( ( is.matrix( y ) | is.vector( y ) ) == FALSE ){ stop( "y must be a vector or a matrix." ) }
  if( ( is.matrix( z ) | is.vector( z ) ) == FALSE ){ stop( "z must be a vector or a matrix." ) }
  if( NROW( y ) != NROW( z ) ){ stop( "The length or number of rows (depending on whether each is a vector or a matrix) of y and z must be the same." ) }

  # If y is a vector,  we need to make it a matrix with a single column.
  if( is.vector( y ) ){ y <- matrix( y, ncol = 1 ) }

  # If x is a vector, it's either a single prediction that's required or the dimension of both x and y is 1.
  if( is.vector( x ) ){
    if( length( x ) == ncol( y ) ){
      x <- matrix( x, nrow = 1 )
    }else{
      if( ncol( y ) == 1 ){
        x <- matrix( x, ncol = 1 )
      }else{
        stop( "If x is a vector, its length must be equal to the number of input dimensions of y (for a single prediction), or y must also have input dimension 1 (in which case it is a vector of points at which to predict at)." )
      }
    }
  }else{
    if( is.matrix( x )  == FALSE ){ stop( "x must be a vector or a matrix." ) }
  }

  # Find the k nearest neighbours in y for each of the points in x.
  NNx <- FNN::get.knnx( data = y, query = x, k = k, algorithm = algorithm )

  # Calculate the inverse of the distances of each point to its nearest k neighbours to the power p.
  inv_dist_p <- 1 / ( NNx$nn.dist^p )

  # If z is a vector...
  if( is.vector( z ) ){

    # Extract the values of z corresponding to the points in NNx.
    z_NNx <- z[NNx$nn.index]

    # Perform k-nearest neighbour interpolation.
    kNN_interpolated_predictions <- rowSums( z_NNx * inv_dist_p ) / rowSums( inv_dist_p )

    # If the distance was NA (as a result of trying to predict at training point), we take the output to be the output at the training point.
    kNN_interpolated_predictions[NNx$nn.dist[,1] == 0] <- z[NNx$nn.index[NNx$nn.dist[,1]==0,1]]

  }else{

    # Extract the values of z corresponding to the points in NNx.
    z_NNx <- array( z[NNx$nn.index,], dim = c( dim( NNx$nn.index ), ncol( z ) ) )

    # Perform k-nearest neighbour interpolation.
    kNN_interpolated_predictions <- colSums( aperm( QuickFunc::add_arrays( z_NNx, inv_dist_p, d = 3, operation = "multiplication" ),
                                                       perm = c(2,1,3) ) ) / rowSums( inv_dist_p )

    # If the distance was NA (as a result of trying to predict at training point), we take the output to be the output at the training point.
    kNN_interpolated_predictions[which(NNx$nn.dist[,1] == 0),] <- z[NNx$nn.index[NNx$nn.dist[,1]==0,1],]

  }

  # Return the result.
  return( kNN_interpolated_predictions )

}
