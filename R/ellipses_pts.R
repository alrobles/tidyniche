#' Get points of the boundary of an ellipse from multivariate environment data
#' @importFrom utils combn
#' @param mu A vector of estimated means
#' @param S A covariance matrix
#' @param alpha the envolving probability boundary
#' @param n number of points for the ellipse boundary
#'
#' @return A list with ellipse points for each variable combination
#' @export
#'
#' @examples
#' pts <- get_env_var(rawSpOccPnts, stack_1_12_19)
#' par <- get_ellip_par(pts)
#' ellipses_pts(par$mu, par$S)
ellipses_pts <- function(mu, S, alpha = .95, n = 100) {
  get_coords_xy <- function(i, j) {
    tmp <- eigen(S[c(i, j), c(i, j)])
    alpha <- .95
    n <- 100
    hlen <- sqrt(qchisq(alpha, df = 2) * tmp$val)
    t <- seq(0, 2 * pi, len = n + 1)
    theta <- atan2(tmp$vec[2, 1], tmp$vec[1, 1])

    x <- hlen[1] * cos(t)
    y <- hlen[2] * sin(t)
    phi <- atan2(y, x)
    rad <- sqrt(x^2 + y^2)
    cbind(
      x = rad * cos(phi + theta) + mu[i],
      y = rad * sin(phi + theta) + mu[j]
    )
  }

  combs <- utils::combn(1:ncol(S), 2, simplify = FALSE)

  list_combs <- Map(f = function(x) get_coords_xy(x[1], x[2]), combs)
  if (length(list_combs) == 1) {
    return(list_combs[[1]])
  } else {
    return(list_combs)
  }
}
