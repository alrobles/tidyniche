#' Negative log likelyhood
#'
#' @param sam1 A matrix containing the original sample of environmental combinations that correspond to presences
#' @param sam2 matrix containing a second random sample of environmental combinations which come from the area of study (M)
#' @param mu A vector mu of parameters
#' @param A matrix as the inverse of covariance matrix
#'
#' @return A negative log likelyhood value
#' @export
#'
#' @examples
#' par <- get_optim_par(spOccPnts)
#' negloglike_multivariable(par$mu, par$A, spOccPnts, samMPts)
negloglike_multivariable <- function(mu, A, sam1, sam2){

  # define the parameters of interest from the guess parameter
  # mu <- guess$mu
  # A <- guess$A

  q1 <- apply(sam1, 1, quad, mu, A) # quadratic terms of presence points
  q2 <- apply(sam2, 1, quad, mu, A) # quadratic terms of M points

  # negative log-likelihood value
  S <- get_negative_log(q1, q2)
  return(S)
}
