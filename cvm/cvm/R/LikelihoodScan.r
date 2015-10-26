#' Likelihood scan function
#' 
#' @param Z track
#' @param T times of observation
#' @param nu,tau,v0 median-ish values of 'nu', 'tau' and 'v0' around which to scan
#' @param FUN a likelihood functions to scan
#' @description returns a list of three vectors: ll.nu, ll.tau. ll.v0 of the likelihoods computed over the values of the respective vector, the other 2 parameters remaining fixed at the respective fixed value. 

LikelihoodScan <- function(Z, T, nu, tau, v0, FUN, ...)
{
  nus <- seq(nu/3,3*nu, length=30)
  ll.nu <- c()
  for(mynu in nus)
    ll.nu <- c(ll.nu, FUN(c(mynu,tau,v0),Z[-1],T[-1], ...))
  
  taus <- seq(tau/5,5*tau,length=30)
  ll.tau <- c()
  for(mytau in taus)
    ll.tau <- c(ll.tau, FUN(c(nu,mytau,v0),Z[-1],T[-1], ...))
  
  v0s <- seq(-v0,5*v0,length=30)
  ll.v0 <- c()
  for(myv0 in v0s)
    ll.v0 <- c(ll.v0, FUN(c(nu,tau,myv0),Z[-1],T[-1], ...))
  
  par(mfrow=c(1,3))
  
  plot(nus, ll.nu, type="l")
  abline(v=nu, col=2, lwd=3, lty=3)
  abline(v=nus[ll.nu == max(ll.nu)], col=3, lwd=2, lty=2)
  
  plot(taus, ll.tau, type="l", ylab=""); abline(v=tau, col=2, lwd=3, lty=3); abline(v=taus[ll.tau == max(ll.tau)], col=3, lwd=2, lty=2)
  
  plot(v0s, ll.v0, type="l", ylab=""); abline(v=v0, col=2, lwd=3, lty=3); abline(v=v0s[ll.v0 == max(ll.v0)], col=3, lwd=2, lty=2)
  
  return(list(ll.nu, ll.tau, ll.v0))
}
