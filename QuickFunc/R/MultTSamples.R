#' Multiple Multivariate t-distribution Samples
#'
#' @description Generate multiple multivariate t-distribution samples given a collection of means and variances.
#'
#' @param Means A vector or matrix of mean values.  If a vector, the means are 1-dimensional given by each element.
#' If a matrix, the mean vectors are given by row.
#' @param Variances Scalar or Matrix if the variances for each sample are the same.  A vector or 3D array if they are required to be different.
#' @param n the size of each sample.
#' @param df the number of degrees of freedom.
#'
#' #' @details This function generates multiple multivariate t-distribution (with \code{df} degrees of freedom) samples, each of size \code{n}, for each mean scalar/vector in \code{Means} and variance scalar/matrix (if variance the same for each Mean) or vector/array \code{Variances} (if the variance is different for each Mean).  Note that there may be ways to improve the efficiency of this function!  In addition, it should only really be used if the there is a (not uncorrelated) multivariate variance structure since the multivariate sampling function is used unless \code{Means} is a vector.
#'
#' @return A matrix containing all the drawn samples (as rows).
#' @export
#'
#' @examples
#' mu <- matrix(c(2,4,5,-1,-1,2), nrow=2)
#' Sigma <- array( c(2, 0.6, -0.4, 0.6, 3, 0.8, -0.4, 0.8,
#' 2.2, 3, 0.6, -0.4, 0.6, 2, 0.8, -0.4, 0.8, 2.1), dim = c(3,3,2) )
#' MultTSamples( mu, Sigma, 4, df = 10 ) # Variances can be an 3D array...
#' MultTSamples( mu, Sigma[,,1], 4, df = 10) # ...or Variances can be a matrix.
#' MultTSamples( mu[1,], Variances = diag(Sigma[,,1]), 5, df = 10 ) # Means can be a vector.
#' MultTSamples( mu[1,], Variances = Sigma[1,1,1], 5, df = 10 )
MultTSamples <- function( Means, Variances, n, df ){

  # Check input conditions.
  if( ( is.matrix( Means ) | is.vector( Means ) ) == FALSE ){ stop( "Means must be a vector or a matrix.") }
  if( n %% 1 != 0 | n <= 0 ){ stop( "n must be an integer greater than 0." ) }

  # Is Means a matrix? - this determines whether it is multivariate or a single variable.
  if( is.matrix( Means ) == TRUE ){

    # Check corresponding input condition for Variances.
    if( is.array( Variances ) == FALSE ){ stop( "If Means is a matrix, Variances must be a (2D or 3D) array.") }
    if( dim( Variances )[1] != dim( Variances )[2] ){ stop( "Variance matrices must be square." ) }

    Z_sample <- matrix( NA, nrow = n * nrow( Means ), ncol = ncol( Means ) )

    # Is Variances a 3D array - i.e. is there a different variance matrix for each mean vector?
    if( length( dim( Variances ) ) == 3 ){

      for( i in 1:nrow( Means ) ){

        try( Z_sample[ ( ( i-1 ) * n + 1 ):( i * n ), ] <- tmvtnorm::rtmvt( n = n, mean = Means[i,], sigma = Variances[,,i], df = df ) )

        if( is.na( Z_sample[ i*n, 1] ) ){ warning( paste( "Matrix", i, "is not a valid variance matrix, hence NAs produced", sep=" ") ) }

      }

    }else{

      # In this case there should be a variance matrix, this being the same for each mean vector.
      for( i in 1:nrow( Means ) ){

        try( Z_sample[ ( ( i-1 ) * n + 1 ):( i * n), ] <- tmvtnorm::rtmvt(n = n, mean = Means[i,], sigma = Variances, df = df) )

        if( is.na( Z_sample[ i*n, 1 ] ) ){ stop( "Matrix Variances was not a valid variance matrix." ) }

      }

    }

  }else{

    # Check corresponding input condition for Variances.
    if( is.vector(Variances) == FALSE ){ stop( "If Means is a vector, Variances must be a scalar or a vector.") }

    # This is the case that Means is given by a vector (that is, it is one-dimensional) - in this case the function returns a vector also.
    Z_sample <- rep( NA, n * length( Means ) )

    # Is Variances a vector (of length greater than 1)?  If so, there is a different variance for each mean.
    if( length( Variances ) > 1 ){

      for(i in 1:length( Means ) ){

        try( Z_sample[ ( ( i-1 ) * n + 1):( i * n ) ] <- sqrt( Variances[i] ) * stats::rt( n, df = df ) + Means[i] )

        if( is.na( Z_sample[ i*n ] ) ){ warning( paste( "Variance", i, "was negative, hence NAs produced", sep=" ") ) }

      }

    }else{

      if( Variances < 0 ){ stop( "Variances must be non-negative." ) }

      # Otherwise, there is a single variance (scalar) for each mean value.
      for(i in 1:length(Means)){

        Z_sample[ ( ( i-1 ) * n + 1 ):( i * n )] <- sqrt( Variances ) * stats::rt( n, df = df) + Means[i]

      }

    }

  }

  return(Z_sample)

}
