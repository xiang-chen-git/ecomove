ComplexDot <- function(a,b)
  return(Re(a*Conj(b)) + Im(a*Conj(b)))

GetVaf <- function(Walk, lag)
{
  dT <- mean(diff(Walk$T))
  lag.scalar <- lag[-1]/dT	
  V <- diff(Walk$Z)	
  
  getVaf <- function(lag)
    mean(ComplexDot(V[-(1:lag)],V[-((length(V)-lag+1):length(V))]))/mean(ComplexDot(V,V))
  
  vaf <- apply(as.matrix(lag.scalar),1,getVaf)
  vaf <- c(1,as.vector(vaf))
  data.frame(lag,vaf)
}


LikelihoodEstimate <- function(T,V, both=FALSE)
  # This is the key likelihood estimating function
  # takes Z,T and V.
  # Either estimates both "tau" and "nu" if "both==TRUE"
  # or just "tau" is "both==FALSE"
  # Returns point estimates and (POSITIVE) log-likelihood
{
  # require(mvtnorm)
  
  V.x1 <- Re(V)[-length(V)]
  V.x2 <- Re(V)[-1]
  V.y1 <- Im(V)[-length(V)]
  V.y2 <- Im(V)[-1]
  
  dT <- diff(T)
  
  f.V <- function(v1, v2, dt, tau, nu)
  {
    v.mean <-  v1 * exp(-dt/tau)
    v.var <- (4/pi) * nu^2 * (1 - exp(-dt/tau))
    dnorm(x = v2, mean = v.mean, sd = sqrt(v.var)) 
  }
  
  V.LogLikelihood.Tau <- function(p)
  {
    tau <- p[1]
    L.x <- f.V(V.x1, V.x2, dt=dT, tau, nu.hat)
    L.y <- f.V(V.y1, V.y2, dt=dT, tau, nu.hat)
    L <- c(L.x, L.y) 
    logL <- sum(log(L  +10^(-10)))
    return(-logL)
  }
  
  V.LogLikelihood.NuTau <- function(p)
  {
    tau <- p[1]
    nu <- p[2]
    L.x <- f.V(V.x1, V.x2, dt=dT, tau, nu)
    L.y <- f.V(V.y1, V.y2, dt=dT, tau, nu)
    L <- c(L.x, L.y) 
    logL <- sum(log(L  +10^(-10)))
    return(-logL)
  }
  
  nu.hat <- mean(Mod(V))
  
  if(!both)
  {
    #tau.hat <- optim(1,V.LogLikelihood.Tau)$par
    tau.hat <- optimize(V.LogLikelihood.Tau,lower=0,upper=max(T)/5)$min
    LL <- -V.LogLikelihood.Tau(tau.hat)
  }
  
  if(both)
  {
    nutau.hat <- optim(c(2,nu.hat), V.LogLikelihood.NuTau, 
                       lower=c(0,0), upper=c(Inf, Inf), method="L-BFGS-B")$par
    nu.hat <- nutau.hat[2]
    tau.hat <- nutau.hat[1]
    LL <- -V.LogLikelihood.Tau(nutau.hat)
  }
  return(data.frame(nu.hat=nu.hat, tau.hat=tau.hat, LL=LL))
}



LikelihoodEstimateOld <- function(Z,T,V, both=FALSE)
  # This is the key likelihood estimating function
  # takes Z,T and V.
  # Either estimates both "tau" and "nu" if "both==TRUE"
  # or just "tau" is "both==FALSE"
  # Returns point estimates and (POSITIVE) log-likelihood
{
  require(mvtnorm)
  
  Z.x1 <- Re(Z)[-length(Z)]
  Z.x2 <- Re(Z)[-1]
  V.x1 <- Re(V)[-length(Z)]
  V.x2 <- Re(V)[-1]
  
  Z.y1 <- Im(Z)[-length(Z)]
  Z.y2 <- Im(Z)[-1]
  V.y1 <- Im(V)[-length(Z)]
  V.y2 <- Im(V)[-1]
  
  dT <- diff(T)
  
  f.Z <- function(z1, z2, v1, dt, tau, nu)
    # distribution function of Z
  {
    z.mean <-  z1 + v1 * tau * (1 - exp(-dt/tau))
    z.var <- (4/pi) * nu^2*tau^2*(dt/tau -2 + exp(-dt/tau)*(dt/tau + 2))
    dnorm(x = z2, mean = z.mean, sd = sqrt(z.var)) 
  }
  
  f.V <- function(v1, v2, dt, tau, nu)
  {
    v.mean <-  v1 * exp(-dt/tau)
    v.var <- (2/pi) * nu^2 * tau * (1 - exp(-2*dt/tau))
    dnorm(x = v2, mean = v.mean, sd = sqrt(v.var)) 
  }
  
  f.VZ <- function(z1, z2, v1, v2, dt, tau, nu)
    # distribution function of Z
  {
    v.mean <-  v1 * exp(-dt/tau)
    v.var <- (4/pi) * nu^2 * tau * (1 - exp(-dt/tau)) 
    
    z.mean <-  z1 + v1 * tau * (1 - exp(-dt/tau))
    
    # ADJUST THESE to reflect our NEW reality
    z.var <- (4/pi) * nu^2*tau^2*(dt/tau - 2 + exp(-dt/tau)*(dt/tau+2))
    # z.var <- (4/pi) * nu^2 * tau^2 * (dt/ tau - 2*(1 - exp(-dt/ tau)) + 1/2 *(1 - exp(-2*dt/ tau)))
    # vz.cov <- (2/pi) * nu^2 * tau * (1 - 2*exp(-dt/tau) + exp(-2*dt/tau))
    
    Mean <- c(v.mean, z.mean)
    Sigma <- rbind(c(v.var, vz.cov), c(vz.cov, z.var))
    
    dmvnorm(x = c(v2, z2), mean = Mean, sigma = Sigma) 
  } 
  
  V.LogLikelihood.Tau <- function(p)
  {
    tau <- p[1]
    L.x <- f.V(V.x1, V.x2, dt=dT, tau, nu.hat)
    L.y <- f.V(V.y1, V.y2, dt=dT, tau, nu.hat)
    L <- c(L.x, L.y) 
    logL <- sum(log(L  +10^(-10)))
    return(-logL)
  }
  
  V.LogLikelihood.NuTau <- function(p)
  {
    tau <- p[1]
    nu <- p[2]
    L.x <- f.V(V.x1, V.x2, dt=dT, tau, nu)
    L.y <- f.V(V.y1, V.y2, dt=dT, tau, nu)
    L <- c(L.x, L.y) 
    logL <- sum(log(L  +10^(-10)))
    return(-logL)
  }
  
  #     Z.LogLikelihood.Tau <- function(p)
  #      {
  #          tau <- p[1]
  #          L.x <- f.Z(Z.x1, Z.x2, V.x1, dt=dT, tau, nu.hat)
  #          L.y <- f.Z(Z.y1, Z.y2, V.y1, dt=dT, tau, nu.hat)
  #          L <- c(L.x, L.y) 
  #          logL <- sum(log(L  +10^(-10)))
  #          return(-logL)
  #      }
  
  #      Z.LogLikelihood.NuTau <- function(p)
  #      {
  #          tau <- p[1]
  #          nu <- p[2]
  #          L.x <- f.Z(Z.x1, Z.x2, V.x1, dt=dT, tau, nu)
  #          L.y <- f.Z(Z.y1, Z.y2, V.y1, dt=dT, tau, nu)
  #          L <- c(L.x, L.y) 
  #          logL <- sum(log(L  +10^(-10)))
  #          return(-logL)
  #      }      
  
  #     VZ.LogLikelihood.Tau <- function(p)
  #      {
  #          tau <- p[1]
  #          L.x <- f.VZ(Z.x1, Z.x2, V.x1, V.x2, dt=dT, tau, nu.hat)
  #          L.y <- f.VZ(Z.y1, Z.y2, V.y1, V.y2, dt=dT, tau, nu.hat)
  #          L <- c(L.x, L.y) 
  #          logL <- sum(log(L  +10^(-10)))
  #          return(-logL)
  #      }
  
  #      VZ.LogLikelihood.NuTau <- function(p)
  #      {
  #          tau <- p[1]
  #          nu <- p[2]
  #          L.x <- f.VZ(Z.x1, Z.x2, V.x1, V.x2, dt=dT, tau, nu)
  #          L.y <- f.VZ(Z.y1, Z.y2, V.y1, V.y2, dt=dT, tau, nu)
  #          L <- c(L.x, L.y) 
  #          logL <- sum(log(L  +10^(-10)))
  #          return(-logL)
  #      }
  
  nu.hat <- mean(Mod(V))
  
  if(!both)
  {
    tau.hat <- optimize(V.LogLikelihood.Tau,lower=0,upper=max(T)/5)$min
    LL <- -V.LogLikelihood.Tau(tau.hat)
  }
  
  if(both)
  {
    nutau.hat <- optim(c(2,nu.hat), V.LogLikelihood.NuTau)$par
    nu.hat <- nutau.hat[2]
    tau.hat <- nutau.hat[1]
    LL <- -V.LogLikelihood.Tau(nutau.hat)
  }
  return(data.frame(nu.hat=nu.hat, tau.hat=tau.hat, LL=LL))
}



EstimateMethod1 <- function(Z,T,lag.max=4)
  # "nu.hat" is mean magnitude of velocity
  # "tau.hat" argument at which VAF is equal to 1/e
{
  e <- exp(1)
  
  V <- Mod(diff(Z))/diff(T)
  nu.hat <- mean(V)
  
  dt <- mean(diff(T))
  
  myvaf <- GetVaf(data.frame(Z,T), seq(0,lag.max,dt))
  vaf <- myvaf$vaf
  lag <- myvaf$lag
  
  # this function gives the interpolated spline of the vaf MINUS 1/e
  # the first zero root of this function is the estimate of tau
  f.vaf <- splinefun(lag, vaf-1/e)
  
  # identify possibly multiple locations where vaf crosses zero
  vaf.test <- abs(vaf-1/e)/(vaf-1/e)
  crosszeros <- which(diff(vaf.test)!=0)+1
  
  tau.hat <- NA
  if(length(crosszeros)>0)
  {
    interval <- c(0,lag[min(crosszeros)])
    tau.hat <- uniroot(f.vaf, interval)$root
  }
  return(data.frame(nu.hat=nu.hat, tau.hat=tau.hat)) 
}

EstimateMethod2A <- function(Z,T)
  # "nu.hat" is mean raw velocity estimate
  # "tau.hat" is obtained using likelihood function to estimate
{
  V <- diff(Z)/diff(T)
  nu.hat <- mean(Mod(V))
  LikelihoodEstimate(T[-1],V,both=FALSE)
}

EstimateMethod2B <- function(Z,T)
  # "nu.hat" is mean raw velocity estimate
  # "tau.hat" is obtained using likelihood function to estimate
{
  V <- diff(Z)/diff(T)
  nu.hat <- mean(Mod(V))
  LikelihoodEstimate(T[-1],V,both=TRUE)
}

EstimateMethod3A <- function(Z,T)
  # "V" is splined
  # "nu.hat" and "tau.hat" are obtained from likelihood function 
{
  V <- GetV.spline(Z,T)
  nu.hat <- mean(Mod(V))
  LikelihoodEstimate(T[-1],V,both=FALSE)
}

EstimateMethod3B <- function(Z,T)
  # "V" is splined
  # "nu.hat" and "tau.hat" are obtained from likelihood function 
{
  V <- GetV.spline(Z,T)
  nu.hat <- mean(Mod(V))
  LikelihoodEstimate(T,V,both=TRUE)
}

EstimateMethod4 <- function(Z,T)
  # get characteristic scale based on CRW
  # match to CVM characteristic scales
  # Works only for regularly sampled data!
{
  e <- exp(1)
  dT <- diff(T)[1]
  
  S <- Mod(diff(Z))
  Phi <- Arg(diff(Z))
  Theta <- diff(Phi)
  
  kappa <- mean(cos(Theta))
  lambda <- mean(S^2)/mean(S)^2
  
  ifelse(kappa > lambda/exp(1),
         tau.hat <- (1 - 1/e) * lambda / (lambda - kappa) * dT,
         tau.hat <- (log(lambda) - 1)/log(kappa)*dT)
  
  nu.hat <- sqrt(pi/8 * (lambda + 2*kappa/(1-kappa)) / (dT * tau.hat)) * mean(S)
  
  return(data.frame(nu.hat=nu.hat, tau.hat=tau.hat)) 
}

