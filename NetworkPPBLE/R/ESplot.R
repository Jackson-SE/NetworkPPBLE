#' ESplot
#'
#' @param fx simulator evaluations / data, given as a vector
#' @param Efx emulator expectations, given as a vector
#' @param Varfx emulator variances, given as a vector
#' @param n number of standard deviations either side of the mean for the range of the vertical arrow bars.
#' @param col_Var colour of the error bars
#' @param col_E colour of the points used to represent emulator expectation
#' @param col_line colour of the line \code{fx} = \code{Efx}
#' @param main main title of the plot
#' @param sub subtitle of the plot
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param cex size of the points, as given by \code{par}.
#' @param cex.axis the magnification to be used for axis annotation relative to the current setting of \code{cex}.
#' @param cex.lab the magnification to be used for the x and y labels relative to the current setting of \code{cex}.
#' @param cex.main The magnification to be used for the main titles relative to the current setting of \code{cex}.
#' @param cex.sub The magnification to be used for sub-titles relative to the current setting of \code{cex}/
#' @param xlim x-axis limits
#' @param ylim y-axis limits
#' @param pch integer specifying a symbol or a single character to be used as the default in plotting points.
#' @inheritParams PMarrows
#' @param export_type type of file that the image should be exported as (if required).  By default this is \code{NULL},
#'                    in which case the required plot is plotted in the R interface.
#' @param filename name of the exported file.
#' @param height height of the image
#' @param width width of the image
#' @param mar a numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of lines of margin to be specified on the four sides of the plot.  The default is \code{c(5, 4, 4, 2) + 0.1}.
#' @param mgp The margin line (in \code{mex} units) for the axis title, axis labels and axis line. Note that \code{mgp[1]} affects title whereas \code{mgp[2:3]} affect \code{axis}. The default is c(3, 1, 0).
#'
#' @return A plot, either exported or in the R interface itself.
#' @export
#'
#' @examples
#' fx <- stats::runif( 10 )
#' Efx <- stats::runif( 10 )
#' Varfx <- abs( stats::rnorm( 10, sd = 0.05 ) )
#' ESplot( fx = fx, Efx = Efx, Varfx = Varfx )
ESplot <- function( fx, Efx, Varfx,
                    n = 3,
                    col_Var = "blue", col_E = "red", col_line = "black",

                    main = NULL, sub = NULL, xlab = "x", ylab = "", # Labels
                    cex = 1, cex.axis = 1, cex.lab = 1, cex.main = 1, cex.sub = 1,  # Size
                    xlim = NULL, ylim = NULL,

                    pch = 16, code = 3, angle = 90, length = 0.05, lwd = 1,

                    export_type = NULL, filename = NA, height = 1000, width = 1000, mar = ( c(5, 4, 4, 2) + 0.1 ), mgp = c(3, 1, 0) ){

  if( is.null( export_type ) ==  FALSE ){

    export_type( file=filename, height=height, width=width )

    graphics::par( mfrow = c(1,1), mar = mar, mgp = mgp )

  }

  graphics::plot( x = fx, y = Efx, type = "n",
                  main = main, sub = sub, xlab = xlab, ylab = ylab, # Labels
                  cex = cex, cex.axis = cex.axis, cex.lab = cex.lab, cex.main = cex.main, cex.sub = cex.sub,  # Size
                  xlim = xlim, ylim = ylim )

  NetworkPPBLE::PMarrows( fx, mean = Efx, Var = Varfx, n = n, col = col_Var,
                          code = code, angle = angle, length = length, lwd = lwd )

  graphics::points( fx, Efx, pch=pch, col=col_E, cex=cex )

  graphics::abline( a=0, b=1, col = col_line )

  if( is.null( export_type ) ==  FALSE ){
    grDevices::dev.off()
  }

}
