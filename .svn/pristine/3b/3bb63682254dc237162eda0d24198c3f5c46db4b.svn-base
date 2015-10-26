#' Var-Cov block of 1-D V and Z
#' 
#' Takes two times (ti, tj) and the CVM parameters, and returns either the 2x2 matrix of the (vi,zi)x(vj,zj) covariances - if \code{getCov.vz}, or only the zi-zj component, i.e. cov(z_i, z_j) - if \code{getCov.zz}.  Remarkably, this covariance does not depend at all on the v0 parameter.
#' 
#' @usage getCov.vz(t1, t2, nu, tau)
#' @usage getCov.zz(t1, t2, nu, tau)
#' @param {t1,t2} two times
#' @param {nu,tau,v0} cvm parameters
#' @aliases getCov.zz
#' @examples
#' getCov.vz(t1=5,t2=5, nu=2, tau=10)
#' getCov.vz(t1=5,t2=10, nu=2, tau=10)
#' getCov.vz(t1=95,t2=100, 2,10)

getCov.vz <-
  function(t1, t2, nu, tau)
  {
    eps1 <- exp(-t1/tau)
    kappa <- exp(-(t2 - t1)/tau)
    
    VarV <- (2 * nu^2/pi) * (1 - eps1^2)
    VarZ <- 4 * nu^2 * tau^2/pi * (t1/tau - 2 * (1 - eps1) + (1 - eps1^2)/2)
    CovVZ <- (2 * nu^2 * tau/pi) * (1 - eps1)^2
    
    Cov <- matrix(0, 2, 2)
    Cov[1, 1] <- kappa * VarV 
    Cov[1, 2] <- CovVZ + tau * (1 - kappa) * VarV 
    Cov[2, 1] <- kappa * CovVZ
    Cov[2, 2] <- VarZ + tau * (1 - kappa) * CovVZ
    return(Cov)
  }



getCov.zz <-
function(t1, t2, nu, tau)
{
  eps1 <- exp(-t1/tau)
  kappa <- exp(-(t2 - t1)/tau)
  
  VarV <- (2 * nu^2/pi) * (1 - eps1^2)
  VarZ <- 4 * nu^2 * tau^2/pi * (t1/tau - 2 * (1 - eps1) + (1 - eps1^2)/2)
  CovVZ <- (2 * nu^2 * tau/pi) * (1 - eps1)^2

  return(VarZ + tau * (1 - kappa) * CovVZ)
}
