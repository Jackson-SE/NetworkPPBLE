#' Maximin Latin Hypercube
#'
#' @description Generate a Maximin Latin hypercube design.  Samples many Latin hypercubes (\code{niter}) and selects
#' the one with maximum minimum distance between any two of its points.
#'
#' @param m number of points in the Latin hypercube.
#' @param d number of dimensions over which the Latin hypercube is required.
#' This can also be implicitly specified as the number of rows of \code{rect}.
#' @param niter Number of Latin hypercubes to sample and out of which the one
#' with maximum minimum distance between any two points will be chosen.
#' @param rect a matrix with \code{d} rows and \code{2} columns indicating the ranges over which the parameters
#' should be chosen.  Can also be specified by the string \code{"MinusoneOne"} if they are [-1,1] (default) or
#' \code{"ZeroOne"} if they are all [0,1].
#' @param plot_best_iterations should the current best iterations be plotted as they are found?
#' @param print_best_iterations should the iteration number of the best iterations be printed as they are found?
#'
#' @return
#' \item{Dx_best}{best design according to the Maximin criterion.}
#' \item{c_D_best}{iteration on which the best design was generated.}
#'
#' @export
#'
#' @examples
#' MLH( m = 10, d = 3 )
MLH <- function( m, d = 2, niter = 1000, rect = "MinusoneOne", plot_best_iterations = TRUE, print_best_iterations = TRUE){

  # Check the input conditions.
  if( m %% 1 != 0 | m <= 0 ){ stop( "m must be a non-negative integer.") }
  if( d %% 1 != 0 | d <= 0 ){ stop( "d must be a non-negative integer.") }

  if( rect[1] == "MinusoneOne" ){
    rect <- matrix( c(-1, 1), nrow = d, ncol = 2, byrow = TRUE )
  }else{
    if( rect[1] == "ZeroOne" ){
      rect <- matrix( c(0, 1), nrow = d, ncol = 2, byrow = TRUE)
    }else{
      if( dim( rect )[[2]] != 2 ){ stop( "rect must be a matrix with 2 columns." ) }
    }
  }

  propose_Dx <- function( m = m, rect = rect ){
    tgp::lhs( n = m, rect = rect )			# use this for lhc sampling.
  }

  # Minimum distance between any two points criterion.
  eval_design_crit <- function( Dx ){
    min( as.vector( stats::dist( Dx ) ) )
  }

  c_D_best <- 0
  niter <- niter
  for( i in 1:niter ){
    ### propose a candidate design Dx ###
    Dx <- propose_Dx( m=m, rect=rect )
    c_D <- eval_design_crit( Dx = Dx )

    ### check if c_D is good and store c_D and Dx if it is ###
    if(c_D > c_D_best) {
      c_D_best <- c_D; Dx_best <- Dx
      #       plot(X[,1:2],xlim=c(0,1),ylim=c(0,1),pch=16,cex=0.7)
      #       points(Dx[,1:2],pch=16,col=2,cex=1.4)
      #       pairs(rbind(X,Dx_best),col=rep(c(1,2),c(nrow(X),m)),cex=rep(c(0.5,1.2),c(nrow(X),m)),pch=16)

      if( plot_best_iterations == TRUE ){
        graphics::pairs( Dx_best, col = 2,cex = 1.2, pch = 16 )
      }
      if( print_best_iterations == TRUE ){
        cat( "c_D =", c_D, ", iter =", i, "\n" )
      }
    }
  }
  return( list( "Dx_best" = Dx_best, "c_D_best" = c_D_best ) )
}
