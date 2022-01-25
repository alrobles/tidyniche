#' Chi square probability  distance
#'
#' @param env A data frame with environmental information according to presence points
#' @param el_pars A list with ellipsoid parameters
#'
#' @return A vector with chi square probabilities from Mahalanobis distance
#'  according to each point in environmental space
#' @export
#' @examples
#' pars <- get_negloglike_optimr_par(spOccPnts, samMPts)
#' get_chi_prob(spOccPnts, pars)
get_chi_prob <- function(env, el_pars){
  mahadist <- get_mahalanobis(env, el_pars)
  p <- pchisq(mahadist, df = ncol(env)-1, lower.tail=FALSE)
  return(p)
}


