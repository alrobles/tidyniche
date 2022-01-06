#' Negative log likelyhood
#'
#' @param guess A vector of length 5 when d=2, it contains the mu and A values as elements
#' @param sam1 A matrix containing the original sample of environmental combinations that correspond to presences
#' @param sam2 matrix containing a second random sample of environmental combinations which come from the area of study (M)
#'
#' @return A negative log likelyhood value
#' @export
#'
#' @examples
#' negloglike(initialValues, spOccPnts, samMPts)
negloglike <- function(guess, sam1, sam2){
  # define the parameters of interest from the guess parameter
  mu <- guess[1:2]
  A <- matrix(c(guess[3], guess[4], guess[4], guess[5]), nrow=2, ncol=2)
  # original sample size: number of presence points in the sample
  n <- nrow(sam1)
  # function that calculates quadratic terms
  quad <- function(xi) {
    ax <- as.matrix(xi - mu);
    t(ax) %*% A %*% ax
  }
  q1 <- apply(sam1, 1, quad) # quadratic terms of presence points
  q2 <- apply(sam2, 1, quad) # quadratic terms of M points
  # negative log-likelihood value
  S <- 0.5*sum(q1) + n*log(sum(exp(-0.5*q2)))
  return(S)
}
