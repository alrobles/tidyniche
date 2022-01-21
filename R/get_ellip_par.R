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
  mu <- colMeans(df, na.rm = TRUE)
  A <-   stats::cov(df)
  par_list <- list(mu = mu, A = A)
  return(par_list)
}

