#' Multiple Normal Samples
#'
#' @description Generate multiple normal samples given a collection of means and variances.
#'
#' @param Means A k by m matrix, k-vector or m-vector of mean values.  See details for information about k and m.
#' @param Variances A k by k by m array, k by k matrix, k by m matrix, k-vector, m-vector or scalar.
#' @param n number of samples required for each mean and variance combination.
#' @param uncorrelated A logical indicating whether each output component should be sampled as though uncorrelated with the others.  This
#' massively reduces the computational time TRUE (as is default).
#'
#' @details This function generates m k-variate normal samples for a collection of means and variances.  These samples may be univariate or multivariate, depending on whether the \code{uncorrelated} is set to be \code{TRUE} or \code{FALSE}.
#'
#' @return A matrix the drawn samples (as rows) appropriately.
#' @export
#'
#' @examples
#' mu <- matrix(c(2,4,5,-1,-1,2), nrow=3)
#' Sigma <- array( c(2, 0.6, -0.4, 0.6, 3, 0.8, -0.4, 0.8,
#' 2.2, 3, 0.6, -0.4, 0.6, 2, 0.8, -0.4, 0.8, 2.1), dim = c(3,3,2) )
#' MultNormSamples( mu, Sigma, 4, uncorrelated = FALSE ) # Variances can be an 3D array...
#' MultNormSamples( mu, Sigma[,,1], 4, uncorrelated = FALSE ) # ...or Variances can be a matrix.
#' MultNormSamples( mu[,1], Variances = diag(Sigma[,,1]), 5 ) # Means can be a vector.
#' MultNormSamples( mu[,1], Variances = Sigma[1,1,1], 5 )
MultNormSamples <- function( Means, Variances, n = 1, uncorrelated = TRUE ){

  # Check input conditions.
  if( ( is.array( Variances ) | is.vector( Variances ) ) == FALSE ){ stop( "Variances must be a vector or a (2D or 3D) array." ) }
  if( ( is.vector( Means ) | is.matrix( Means ) ) == FALSE ){ stop( "Means must be a vector or matrix." ) }

  # If Variances is a vector, then the output components can't be correlated, however, if uncorrelated is FALSE,
  # the method can go ahead with uncorrelated switched to TRUE as it makes no difference.
  if( is.vector( Variances ) & uncorrelated == FALSE ){ uncorrelated <- TRUE }

  # Conversely, if Variances is an array and uncorrelated is set to TRUE, uncorrelated will be set to FALSE with a warning.
  if( is.array( Variances ) & length( dim( Variances ) ) == 3 & uncorrelated == TRUE ){
    uncorrelated <- FALSE
    warning( "Variances is a 3D array, hence uncorrelated has been automatically set to FALSE." )
  }

  # First we consider the case that the output components are not uncorrelated.
  if( identical( uncorrelated, FALSE ) ){

    # And the case when each sample has a different variance matrix specified.
    if( length( dim( Variances ) ) == 3 ){

      # Check that the dimension of the variance array is OK.
      if( dim( Variances )[1] != dim( Variances )[2] ){
        stop( "The first and second dimension of the Variances array (if 3D) must be equal." ) }

      # Obtain k (number of output components) and m (number of points at which samples will be taken).
      k <- dim( Variances )[1]
      m <- dim( Variances )[3]

      # Check that the mean matrix is commensurate with the size of the variance array.
      if( identical( dim( Means ), c(k,m) ) == FALSE ){
        stop( "Dimension of Means matrix not consistent with the dimension of the Variances array.") }

      # Generate an empty (n x k x m) array to put the samples into.
      Z_samples <- array( NA, dim = c(n,k,m) )

      # Generate a normal sample for each of the m means and variances.
      for( i in 1:m ){

        try( Z_samples[,,i] <- MASS::mvrnorm( n = n, mu = Means[,i], Sigma = Variances[,,i] ) )

        if( is.na( Z_samples[1,1,i] ) ){ warning( paste( "Matrix", i, " of Variances is not a valid variance matrix, hence NAs produced", sep=" ") ) }

      }

    }else{

      # Obtain k and m.
      k <- NROW( Means )
      m <- NCOL( Means )

      # Check that the mean matrix/vector is commensurate with the size of the variance matrix.
      if( identical( dim( Variances ), c(k,k) ) == FALSE ){
        stop( "The number of rows and columns of the variance matrix must be equal, and equal to the number of elements/rows of the Means vector/matrix." ) }

      # First, if Means is not a vector.
      if( is.vector( Means ) == FALSE ){

        # Generate an empty (n x k x m) array to put the samples into.
        Z_samples <- array( NA, dim = c(n,k,m) )

        # Generate a normal sample for each of the m means.
        for( i in 1:m ){

            try( Z_samples[,,i] <- MASS::mvrnorm( n = n, mu = Means[,i], Sigma = Variances ) )

            if( is.na( Z_samples[i,i,1] ) ){ stop( "Matrix Variances is not a valid variance matrix." ) }

        }

      # Otherwise, we assume that Means is a vector.
      }else{

        # in which case, we just have one multivariate normal sample to take.
        Z_samples <- MASS::mvrnorm( n = n, mu = Means, Sigma = Variances )

      }

    }

  # Otherwise we assume that the output components are not correlated, in which case we can make use of the rnorm function alone.
  }else{

    # Check the objects Means and Variances are commensurate sizes.
    if( ( is.vector( Means ) | is.matrix( Means ) ) == FALSE ){
      stop( "Variances must be a vector or matrix if outputs are to be sampled as uncorrelated." ) }
    if( NROW( Means ) != NROW( Variances ) & length( Variances ) != 1 ){
      stop( "Means and Variances objects are not commensurate with each other." ) }

    # We can now generate an rnorm sample of the appropriate size (n x k x m).
    random_normal_sample <- stats::rnorm( n = n * NROW( Means ) * NCOL( Means ) )

    # If Means is a matrix.
    if( is.matrix( Means ) ){

      # Get m and k.
      k <- NROW( Means )
      m <- NCOL( Means )

      # Scale Z_sample appropriately - shouldn't matter is Variances is vector or matrix.
      Z_samples <- array( random_normal_sample * rep( c( sqrt( Variances ) ), each = n ) + rep( Means, each = n ) , dim = c(n,k,m) )

    # Else Means must be a vector.
    }else{

      # Scale Z_sample appropriately - shouldn't matter is Variances is vector or scalar.
      Z_samples <- t( matrix( random_normal_sample * sqrt( Variances ) + Means, ncol = n ) )

    }

  }

  # Return the result.
  return( Z_samples )

}
