#' Likelihood of 1-D Z estimation
#' 
#' Likelihood for estimating cvm parameters for one dimensional Z vector. 
#' 
#' @aliases MinMe, Estimate, RightSide
#' @examples 
#' # Obtain marginal likelihoods of the parameters 
#' nu <- 10
#' tau <- 3
#' v0 <- 20
#' T <- cumsum(rexp(50))
#' Z <- Re(CVM2(nu, tau, T, v0)$Z)
#' 
#' nus <- seq(nu/3,3*nu, length=30)
#' ll.nu <- c()
#' for(mynu in nus)
#'   ll.nu <- c(ll.nu, Z.likelihood(c(mynu,tau,v0),Z,T))
#' 
#' taus <- seq(tau/5,5*tau,length=30)
#' ll.tau <- c()
#' for(mytau in taus)
#'   ll.tau <- c(ll.tau, Z.likelihood(c(nu,mytau,v0),Z,T))
#' 
#' v0s <- seq(-v0,5*v0,length=30)
#' ll.v0 <- c()
#' for(myv0 in v0s)
#'   ll.v0 <- c(ll.v0, Z.likelihood(c(nu,tau,myv0),Z,T))
#'
#' # Plot the marginal likelihoods
#'
#' par(bty="n", pch=19, mfrow=c(1,3))
#' plot(nus, ll.nu, type="l"; abline(v=nu, col=2, lwd=3, lty=3); abline(v=nus[ll.nu == max(ll.nu)], col=3, lwd=2, lty=2)
#' 
#' plot(taus, ll.tau, type="l", ylab=""); abline(v=tau, col=2, lwd=3, lty=3); abline(v=taus[ll.tau == max(ll.tau)], col=3, lwd=2, lty=2)
#' 
#' plot(v0s, ll.v0, type="l", ylab=""); abline(v=v0, col=2, lwd=3, lty=3); abline(v=v0s[ll.v0 == max(ll.v0)], col=3, lwd=2, lty=2)
#'
#' # Obtain MLE
#' 
#' nu <- 3; tau <- 0.5; v0 <- 0
#' T <- cumsum(rexp(50))
#' Z <- Re(CVM2(T, nu, tau, v0)$Z)
#' plot(T,Z, type="o")
#' 
#' optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))



Z.likelihood <-
function(p, Z, T)
{
  v0 <- diff(Z)[1]/diff(T)[1]
  nu <- p[1]; tau <- p[2]
  Sigma.zz <- getSigma.ZZ(T, nu, tau)
  mu.z <- v0*tau*(1-exp(-T/tau))
  dmvnorm(Z,mu.z,Sigma.zz, log=TRUE)
}
