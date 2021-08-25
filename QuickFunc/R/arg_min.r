#' Arg min for monotonically increasing functions.
#'
#' @description Function to find minimum argument for which function is at least as big as some value v, within some accepted precision.
#'
#' @param f function
#' @param v value
#' @param l initial lower bound
#' @param u initial upper bound
#' @param p precision
#'
#' @return \code{a}, a scalar such that \code{f(a) >= v} and \code{f(a-p) < v}, along with \code{fa = f(a)}.
#' @export
#'
#' @examples
#' sam <- sort( sample( 1:100, 10 ) )
#' fn <- function( x ){ if( x >= sam[1] ){ sam[sum( x >= sam )] }else{ 0 } }
#' arg_min( fn, v = 33, l = 0, u = 100, p = 10 )
#' arg_min( fn, v = 33, l = 0, u = 100, p = 0.1 )
#' # In the above example we know of course that the answer should be
#' # minimum of sam values that are greater than v.
arg_min <- function( f, v, l, u, p ){

  # Evaluate f(l)...
  fl <- f( l )

  # ...and check that f(l) < v.
  if( fl >= v ){

    a <- l
    fa <- fl

    warning( "f(l) >= v, hence algorithm stops immediately." )

  # Otherwise...
  }else{

    # Evaluate f(u)...
    fu <- f( u )

    # ...and check that f(u) >= v.
    if( fu < v ){

      a <- NA
      fa <- NA

      warning( "f(u) < v, hence we cannot find a such that l <= a <= u and f(a) >= v, hence NA returned.")

    # Otherwise...
    }else{

      # Whilst the difference between l and u is greater than the desired precision, p...
      while( u - l > p ){

        # ...evaluate the midpoint between l and u.
        m <- ( l + u ) / 2

        # Evaluate the function at this midpoint.
        fm <- f( m )

        # If f(m) >= v...
        if( fm >= v ){

          # ...bring the upper bound down to the midpoint as we know our value is between lower and mid.
          u <- m
          fu <- fm

        # Otherwise...
        }else{

          # ...bring the lower bound up to the midpoint as we know our value is between mid and upper.
          l <- m
          fl <- fm

        }

      }

      # Answer is now the current u, for which we know f(u) >= v, with precision u - l, since whilst f(l) < v, for values between l < s < u, we do not know whether f(s) >= v or not, so our desired answer could be any s in that range.
      a <- u
      fa <- fu

    }

  }

  # Return the result.
  return( list( "a" = a, "fa" = fa ) )

}
