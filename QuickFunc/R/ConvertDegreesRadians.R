#' Convert Degrees to Radians
#'
#' @param d angle in degrees
#'
#' @return angle in radians
#' @export
#'
#' @examples
#'deg2rad( 60 )
deg2rad <- function( d ){

  (pi * d) / 180

}

#' Convert Radians to Degrees
#'
#' @param r angle in radians
#'
#' @return angle in degrees
#' @export
#'
#' @examples
#' rad2deg( 1 )
rad2deg <- function( r ){

  (180 * r) / pi

}
