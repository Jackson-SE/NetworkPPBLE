#' Plot plus-and-minus standard deviation Arrows
#'
#' @param x vector of x coordinates
#' @param mean vector of expected values
#' @param Var vector of variances
#' @param n number of standard deviations for the vertical arrows to be covering.
#' @inheritParams graphics::arrows
#'
#' @return plots some arrows
#' @export
#'
#' @examples
#' x <- stats::runif( 10 )
#' mean <- stats::runif( 10 )
#' Var <- abs( stats::rnorm( 10, sd = 0.05 ) )
#' plot( x = x, y = mean, type="n" )
#' PMarrows( x = x, mean = mean, Var = Var )
PMarrows <- function( x, mean, Var, n = 3, code = 3, angle = 90, length = 0.05, col = 'black', lwd = 1 ){

  # Simply using the arrows function in package graphics to plot some vertical error bars around a set of points.
  graphics::arrows( x0 = x, y0 = mean - sqrt(Var) * n,
                    x1 = x, y1 = mean + sqrt(Var) * n,
                    code = code, angle = angle, length = length, col = col, lwd = lwd )

}
