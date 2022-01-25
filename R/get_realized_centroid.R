#' Environmental centroid
#' Get environmental centroid according to raw points
#'
#' @param df presence points from species
#' @param env A raster stack of environmental information
#'
#' @return A vector with the centroid coordinate from environmental information
#' @export
#'
#' @examples
#' get_realized_centroid(rawSpOccPnts, stack_1_12_19 )
get_realized_centroid <- function(df, env){
  dens <- get_density(df, env)
  dens[which.max(dens[ , ncol(dens)]), -ncol(dens)]
}
