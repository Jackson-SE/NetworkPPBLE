#' Test Data Partitions
#'
#' @description Find the partitions of a set of test data as given by a Treed Gaussian process having used, for example, \code{\link[tgp]{btgp}}
#'
#' @param x Matrix of input points to partition.
#' @param tgp_object An object of the class \code{tgp}.
#'
#' @return Submatrices of \code{x}, partitioned according to the partitions of \code{tgp_object}.
#' @export
#'
#' @seealso
#' \code{\link[tgp]{btgp}}
#'
#' @examples
#' # construct some 1-d nonstationary data - this exammple uses one of
#' # those found in the tgp package itself.
#' X <- seq( 0, 20, length = 100 )
#' XX <- seq( 0, 20, length = 99 )
#' Z <- ( sin( pi * X / 5 ) + 0.2 * cos( 4 * pi * X / 5 ) ) * ( X <= 9.6 )
#' lin <- X > 9.6;
#' Z[lin] <- -1 + X[lin] / 10
#' Z <- Z + rnorm( length( Z ), sd = 0.1 )

#' out <- tgp::btgp( X = X, Z = Z, XX = XX ) 	# use a treed GP
#' plot( out ) 			# plot the surface
#' tgp::tgp.trees( out )
#' partition.test( x = matrix( XX, ncol = 1 ), tgp_object = out )
partition.test <- function( x, tgp_object ){

  # Training data.
  X <- tgp_object$X

  # Partition the training data.
  partitioned.design <- tgp::partition( X, tgp_object )

  # length of the partitioned.design
  lpd <- length( partitioned.design )

  # Empty matrix, into which we shall put two of each row of the partitioned design matrix.  This will ensure that, even if the test matrix is empty, each partition will be labelled correctly.
  Xapp <- matrix( NA, nrow = 2 * lpd, ncol = ncol( X ) )
  for( i in 1:lpd ){
    Xapp[( 2*i - 1 ):( 2*i ),] <- partitioned.design[[i]][1:2,]
  }

  # Save so we can see where each input ends up.
  rownames( x ) <- 1:nrow( x )

  # Partitions the joint collection of x and Xapp.
  x_bind_partitions <- tgp::partition( rbind( Xapp, as.matrix( x ) ), tgp_object )

  # Now get rid of the additional two points in each of the partitions that we added.
  x_test_partitions <- list()
  for( i in 1:lpd ){
    x_test_partitions[[i]] <- x_bind_partitions[[i]][-(1:2),,drop = FALSE]
  }

  # Return the partitions.
  return( x_test_partitions )

}
