#' Number of function evaluations.
#'
#' @description Related to function \code{\link{arg_min}}, this function simply calculates the number of required evaluations of \code{f} to ensure the chosen precision \code{p}, where \code{p} in this case is taken to be a proportion of \code{u-l}.
#'
#' @param p precision
#'
#' @return number of required evaluations
#' @export
#'
#' @examples
#' arg_min_eval_count( 0.0001 )
arg_min_eval_count <- function( p ){

  ceiling( log2( 1 / p ) + 2 )

}
