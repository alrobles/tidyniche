
#' get_env_var
#' Function to extract environmental values from presence points
#'
#' @param env A raster with environmental layers. Must have a projection string
#' @param df A data frame with presence points
#' @importFrom  sp SpatialPointsDataFrame
#' @importFrom raster extract crs
#' @export
#'
#' @examples
#' get_env_var(rawSpOccPnts, stack_1_12)

get_env_var <- function(df, env){
  sp.occpnts0 <-  sp::SpatialPointsDataFrame(coords = df[, 2:3],
                                             data = df,
                                             proj4string = raster::crs(env) )
  sp.occpnts1 <- raster::extract(x = env, sp.occpnts0)
  sp.occpnts1
}
