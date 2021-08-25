#' Mean and standard deviation plot (against continuous x-variable).
#'
#' @description Plot the mean and standard deviation curves over a continuous x-variable (typically time).
#'
#' @param x Vector of x-values (typically representing say, time).
#' @param fx Matrix of true model output across \code{x}, if known (by default \code{NULL}.
#' @param Efx Matrix of emulator expectation across \code{x}.
#' @param Varfx Matrix of emulator variance across \code{x}.
#' @param col_Efx Colour of mean line.
#' @param col_fx Colour of true model line.
#' @param col_sd Colour of standard deviation line.
#' @param lwd line widths.
#' @param lty_Efx Line type for mean line.
#' @param lty_fx Line type of true model line.
#' @param lty_sd Line type of standard deviation line.
#' @param ylim Limits on \code{y}-axis - can be given as an \code{n} by \code{2} matrix if different limits are required for each plot.
#' @param xlim Limits on \code{x}-axis - can be given as an \code{n} by \code{2} matrix if different limits are required for each plot.
#' @param xlab \code{x}-axis label.
#' @param ylab \code{y}-axis label.
#' @param ca Font size of axis.
#' @param cl Font size of labels.
#' @param main vector of plot titles if required.
#' @param export_type type of file that the image should be exported as (if required).  By default this is \code{NULL},
#'                    in which case the required plot is plotted in the R interface.
#' @param filename name of the exported file.
#' @param height height of the image
#' @param width width of the image
#' @param mar a numerical vector of the form \code{c(bottom, left, top, right)} which gives the number of lines of margin to be specified on the four sides of the plot.  The default is \code{c(5, 4, 4, 2) + 0.1}.
#' @param mgp The margin line (in \code{mex} units) for the axis title, axis labels and axis line. Note that \code{mgp[1]} affects title whereas \code{mgp[2:3]} affect \code{axis}. The default is c(3, 1, 0).
#' @param mfrow The number of rows and columns to divide the device window into, given as a vector of two elements.
#'
#' @return Plot.
#' @export
#'
#' @examples
#' x = 1:100
#' Efx = matrix( c( x^(2.1/2), x^(2.2/2) ), nrow = 2, byrow=TRUE )
#' Varfx = matrix( c(x,x), nrow = 2, byrow = TRUE)
#' mean_sd_plot( x = x, Efx = Efx, Varfx = Varfx )
mean_sd_plot <- function( x, fx = NULL, Efx, Varfx,

                          col_Efx = 1, col_fx = 2, col_sd = 3, lwd = 2, lty_Efx = 1, lty_fx = 1, lty_sd = 1,
                          ylim = c(0, max( Efx )), xlim = c( min( x ), max( x ) ), xlab = "", ylab = "", ca = 1, cl = 1,
                          main = rep( "", nrow( Efx ) ),

                          export_type = NULL, filename = NA, height = 1000, width = 1000, mar = ( c(5, 4, 4, 2) + 0.1 ), mgp = c(3, 1, 0), mfrow = c(1,1) ){

  if( is.null( export_type ) ==  FALSE ){

    export_type( file=filename, height=height, width=width )

    graphics::par( mfrow = mfrow, mar = mar, mgp = mgp )

  }

  if( is.vector( xlim ) == TRUE ){
    xlim <- matrix( rep( xlim, NROW( Efx ) ), ncol = 2, byrow = TRUE )
  }

  if( is.vector( ylim ) == TRUE ){
    ylim <- matrix( rep( ylim, NROW( Efx ) ), ncol = 2, byrow = TRUE )
  }

  for( i in 1:NROW( Efx ) ){

    graphics::plot( x, Efx[i,], type = "l", lty = lty_Efx, col = col_Efx, lwd = lwd, ylim = ylim[i,], xlim = xlim[i,], cex.lab = cl, cex.axis = ca, xlab = xlab, ylab = ylab, main = main[i] )
    graphics::lines( x, sqrt( Varfx[i,] ), col = col_sd, lty = lty_sd )
    if( is.null( fx ) == FALSE ){ graphics::lines( x, fx[i,], lty=lty_fx, col = col_fx, lwd=lwd ) }

  }

  if( is.null( export_type ) ==  FALSE ){
    grDevices::dev.off()
  }

}
