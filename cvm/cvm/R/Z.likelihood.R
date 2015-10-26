#' Likelihood functions for cvm estimation
#' 
#' Likelihood for estimating cvm parameters for one dimensional Z vector. 
#' @usage Z.like1D(p, Z, T)
#' @usage Z.like2D(p, Z, T)
#' @usage Z.likeBlock(p, Z, T, v0=NULL, k)
#' @aliases Z.like2D, Z.likeBlock
#' @param p numeric vector of either form c(nu, tau, v0) or c(nu, tau) if v0 is provided
#' @param Z,T one-dimensional location and time vectors
#' @examples 
#' 
#' # Simulate some 1-D data
#' 
#' nu <- 10
#' tau <- 3
#' v0 <- 20
#' mycvm <- CVM2(nu, tau, T = cumsum(rexp(50)), v0)
#' X <- Re(myvm$Z)
#' T <- mycvm$T
#' 
#' # Obtain and plot marginal likelihoods with correct values
#' 
#' LikelihoodScan(X,T,nu,tau,v0,Z.like1)
#' LikelihoodScan(X,T,nu,tau,v0,Z.like2, k=5)
#' 
#' # Obtain MLE
#' 
#' nu <- 3; tau <- 0.5; v0 <- 0
#' T <- cumsum(rexp(50))
#' Z <- Re(CVM2(T, nu, tau, v0)$Z)
#' plot(T,Z, type="o")
#' 
#' optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))


#Z.like1D <-
#  function(p, Z, T, v0=NULL)
#  {
#    nu <- p[1]; tau <- p[2]; if(is.null(v0)) v0 <- p[3]
#    Sigma.zz <- getSigma.ZZ(T, nu, tau)
#    mu.z <- v0*tau*(1-exp(-T/tau))
#    dmvnorm2(Z,mu.z,Sigma.zz, log=TRUE)
#  }

# Z.like2D <-
#   function(p, Z, T, v0=NULL)
#   {
#     nu <- p[1] 
#     tau <- p[2] 
#     if(is.null(v0)) { v0x <- p[3]; v0y <- p[4] } else{v0x <- Re(v0); v0y <- Im(v0)}
#     
#     Sigma.zz <- getSigma.ZZ(T, nu, tau)
#     mux <- v0x * tau*(1-exp(-T/tau))
#     muy <- v0y * tau*(1-exp(-T/tau))
#     
#     dmvnorm2(Re(Z),mux,Sigma.zz, log=TRUE) + dmvnorm2(Im(Z),muy,Sigma.zz, log=TRUE)
#   }


Z.likeBLOCK <- function(p, Z, T, k, v0=NULL)
{
  n <- length(Z)
  nu <- p[1]
  tau <- p[2]
  if(is.null(v0)) v0 <- p[3]
  
  Ls <- rep(0, n-k-1)
  for(i in 1:(length(Ls)))
  {
    mySigma <- getSigma.ZZ(T[(i+1):(i+k)], nu, tau, t0=T[i])
    myMu <- v0*exp(-T[i]/tau) * tau * (1 - exp(-T[(i+1):(i+k)]/tau))
    Ls[i] <- dmvnorm2(Z[(i+1):(i+k)] - Z[i], myMu, mySigma, log = TRUE)
  }
  sum(Ls)
}