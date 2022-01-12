
#' Get ellipsoid parameters. A function to compute average and the inverse of covariance matrix from environmental data
#'
#' @param df A data frame containing environmental variables
#'
#' @return A list with computed average of envinronmental variables and the inverse of covariance matrix
#' @export
#'
#' @examples
#' get_ellip_par(spOccPnts)
get_ellip_par <- function(df){
  #is.numeric(spOccPnts)
  mu <- colMeans(df, na.rm = TRUE)
  # for A:
  A <-   get_A_matrix(df)
  # whole vector of inicial values
  par_list <- list(mu = mu, A = A)
  return(par_list)
}

