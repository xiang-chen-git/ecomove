#' Biased Correlated Random Walk functions
#' 
#' Generates a discrete 2D correlated random walk that is biased towards a given center point. 
#' 
#' @aliases BCRW, multiBCRW
#' @usage BCRW(n = 50, a = 2, b = 1, rho = 0.95, Z.center = 0, attraction = 0.5, Z0 = 0)
#'  multiBCRW(rhos, attractions, Z.centers, ns, ...)
#' 
#' @details The step lengths of this BCRW have a Weibull(shape = a, scale = b) distribution, the turning angles have a wrapped Cauchy (mu_i, rho) distribution, where mu_i is given by:   (1 - A) phi_i + A theta_{i}, where phi_i is the orientation of the previous step and theta_i represents the angle pointed towards the center of attraction.  When the attraction parameter A = 1, the movement is always towards the center of attraction.  If A = 0, the movement is a standard unbiased CRW. 
#' 
#' @details multiBCRW() generates a path composed of multiple BCRW tracks.
#' 
#' @param n number of steps 
#' @param {a,b} shape and scale parameters of the Weibull distribution
#' @param rho clustering parameter for the turning angles
#' @param Z.center center of attraction (complex)
#' @param attraction strength of attraction
#' @param Z0 initial location.  
#' @param {ns,rhos,attractions,Z.centers}  vector of lengths, clustering coeffiecients, attraction coefficients, and center locations (multiBCRW)
#' @param ... additional arguments for BCRW function (multiBCRW)
#' @return BCRW returns a trajectory - as a (`track' class) data frame containg Z (complex vector), X, and Y locations. 
#'
#' @return `multiBCRW' returns a `multipath' class object containing a vector Z of locations and a Phase vector enumerating the phases of the locations. The multipath object can be plotted with the `plot.multipath' method
#' 
#' @examples
#' # BCRW examples
#' par(mfrow=c(2,2), bty="l", mar=c(4,4,1,1))
#' plot(BCRW(n = 500, rho=0.9, attraction = 0.9))
#' plot(BCRW(n = 500, rho=0.9, attraction = 0.2))
#' plot(BCRW(n = 500, rho=0.2, attraction = 0.9))
#' plot(BCRW(n = 500, rho=0.2, attraction = 0.2))
#' 
#' 
#' # multiBCRW examples
#' multiZ <- multiBCRW(c(0.9, 0.2, 0.2, 0.9), c(0.9, 0.2, 0.9, 0.2), c(0, 10-10i, 20+10i, 0), ns = c(100,100,100,100))
#' par(mfrow=c(1,1))
#' plot(multiZ)

BCRW <- function(n = 50, a = 2, b = 1, rho = 0.95, Z.center = 0, attraction = 0.5, Z0 = 0)
{
  Z <- c(Z0, rep(NA,n-1))
  phi <- runif(1, -pi, pi)
  for(i in 2:n)
  {
    chi <- Arg(Z.center - Z[i-1])
    #if(chi*phi >= 0)   
    dphi <- (chi-phi) #else
    if(abs(chi - phi) > pi) dphi <- (chi - phi) - pi
    
    location <- phi + attraction * dphi
    # pick a new absolute direction ... but MUST BE BETWEEN -pi and pi
    phi <- rwrpcauchy(1, location, rho) - 2*pi
    if(phi > pi) phi <- phi-2*pi
    if(phi < -pi) phi <- phi+2*pi
    
    Z[i] <- Z[i-1] + complex(arg = phi, mod = rweibull(1, a, b))
  }
  Z.bcrw <- data.frame(Z, X = Re(Z), Y = Im(Z))
  class(Z.bcrw) <- "track"
  return(Z.bcrw)
}

multiBCRW <- function(rhos, attractions, Z.centers, ns, ...)
{
  Z.crw.list <- list()
  for(i in 1:length(ns))
  {
    if(i == 1) Z0 = 0 else Z0 = tail(Z.crw.list[[i-1]],1)
    Z.crw.list[[i]] <- BCRW(attraction = attractions[i], rho = rhos[i], Z.center = Z.centers[i], n = ns[i], Z0 = Z0, ...)$Z[-1]
  }
  
  Z.crw <- eval(parse(text = paste("c(", paste("Z.crw.list[[",1:length(ns),"]]", sep="", collapse=","), ")")))
  
  Phases <- rep(1:length(ns), ns)
  multipath <- list(Z=Z.crw, Phase=Phases, ns = ns)
  class(multipath) <- "multipath"
  return(multipath)
}
