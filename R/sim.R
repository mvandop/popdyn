



#'Simulation of Predator and Prey Dynamics
#'
#' @param H0 Hare initial population
#' @param P0 Lynx initial population
#' @param a Hare intercept parameter
#' @param b Hare slope parameter
#' @param c Lynx intercept parameter
#' @param d Lynx slope parameter
#' @param t Time (number of iterations)
#'
#' @return data.frame of hare and lynx populations
#' @export
#' @importFrom tibble tibble
#'
#' @example sim()
#'
#'
sim <- function(H0 = 2, P0 = 2, a = 0.01, b = 0.01, c = 0.01, d = 0.01, t = 1000) {
  #initialize
  H <- numeric(t)
  P <- numeric(t)
  H[1] <- H0
  P[1] <- P0
  #loop
  for (i in 1:(t - 1)) {
    H[i + 1] = H[i] + H[i]*harepopfun(H[i], P[i], a, b)
    P[i + 1] = P[i] + P[i]*lynxpopfun(H[i], P[i], c, d)
  }
  #output
  return(tibble::tibble(time = 1:t, hare = H, lynx = P))
}


lynxpopfun <- function(H,P,c,d) {
  (c*H)-d
}

harepopfun <- function(H,P,a,b) {
  a-(b*P)
}
