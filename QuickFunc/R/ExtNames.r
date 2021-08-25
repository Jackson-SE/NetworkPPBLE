#' ExtNames
#'
#' @param L List of objects with names which we wish to extract.
#' @param pos Position for setting where the objects will be extracted to, in terms of environment.
#'
#' @return Objects, to the specified environment.
#' @export
#'
#' @examples
#' L <- list( "rabbit" = 3, "BigMatrix" = matrix( runif( 100 ), ncol = 10 ) )
#' L$rabbit
#' ExtNames( L )
#' rabbit
ExtNames <- function( L, pos = 1 ){

  # Take the names of the objects in the list in turn and assign them as objects in the specified environment.
  lapply( names( L ), function( nm ){ assign( nm, L[[nm]], pos = pos ) } )

}
