#' get_negloglike_optim_par
#' Function  to optime negloglike function given parameters from presence points and a sample
#' of environmental points from a M hypothesis. Returns a list of
#'
#' @param M_pts A dataframe with a sample of environmental values inside an M hypothesis region.
#' @param env_pts A dataframe with environmental variables extracted from presence points
#' @importFrom  utils as.relistable relist
#' @return A list with optimized parameters for negloglike function
#' @export
#'
#' @examples
#' get_negloglike_optim_par(spOccPnts, samMPts)
get_negloglike_optim_par <- function(env_pts, M_pts){
  par <- get_optim_par(env_pts)
  initial.param <- as.relistable(par)
  ul <- unlist(initial.param)
  guess <- relist(ul)
  like.fn <- function(param.vector)
  {
    param <- relist(param.vector, skeleton = par)
    negloglike_multivariable(param$mu, param$A, sam1 = env_pts, sam2 = M_pts)
  }
  find.mle <- optim(par = unlist(par), fn = like.fn, method="Nelder-Mead" )
  mle.par <- relist(find.mle$par, par)
  mle.par$A <- chol2inv(chol(mle.par$A))
  return(mle.par)
}
