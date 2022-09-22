#' Sample environmental points from M hypothesis
#'
#' @param M.shp A shapefile with an M hypothesis of the species
#' @param N a numeric with the number of points of the sample, default is 10 000
#' @param env an stack of raster with environmental variables to extract information
#' @importFrom raster crop mask
#' @return a data frame with a sample of environmental values inside the M polygon
#' @export
#'
#' @examples
#' sam_polyM(M.shp = Mshp, N = 100, env = stack_1_12 )
sam_polyM <- function(M.shp, env, N = 10000){
  # crop and mask the environmental layers with the M polygon
  crop.M <- raster::crop(env, M.shp)
  clip.M <- raster::mask(crop.M, M.shp)
  # get ride of cells with NA values
  ind <- which(!is.na(clip.M[[1]][]))
  # get a random sample of indices
  sam <- sample(ind, N, replace = TRUE)
  # choose the points corresponding to the selected indices
  Mpnts <- clip.M[][sam, ]
  return(Mpnts)
}
