#' get_negloglike_optim_par
#' Function  to optime negloglike function given parameters from presence points and a sample
#' of environmental points from a M hypothesis. Internally runs Returns a list of parameters
#'
#' @param M_pts A dataframe with a sample of environmental values inside an M hypothesis region.
#' @param env_pts A dataframe with environmental variables extracted from presence points
#' @param lower logical if is set create a low bowndarie for parameters from M points
#'
#' @importFrom  utils as.relistable relist
#' @import optimr
#' @return A list with optimized parameters for negloglike function
#' @export
#'
#' @examples
#' MPts <- sam_polyM(Mshp, stack_1_12, 500)
#' get_negloglike_optimr_par(head(spOccPnts, 30), MPts)

get_negloglike_optimr_par <- function(env_pts, M_pts, lower = FALSE){
  par <- get_ellip_par(env_pts)
  initial.param <- as.relistable(par)
  ul <- unlist(initial.param)
  guess <- relist(ul)
  like.fn <- function(param.vector)
  {
    param <- relist(param.vector, skeleton = par)
    negloglike_multivariable(param$mu, param$S, sam1 = env_pts, sam2 = M_pts)
  }

  if(lower == TRUE){
    min.par <- get_ellip_par(M_pts)
    find.mle <- optimr::optimr(par = unlist(par), fn = like.fn,
                               lower = unlist(min.par),
                               method="Rvmmin" )
  } else {
    find.mle <- optimr::optimr(par = unlist(par), fn = like.fn,
                               method="Rvmmin" )

  }

  mle.par <- relist(find.mle$par, par)
  return(mle.par)
}
