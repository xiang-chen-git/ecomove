estimateCVM.vaf <- function(Z, T, CI = FALSE, diagnose = FALSE, spline=FALSE)
{
  T <- T-min(T)
  dT <- mean(diff(T))
  if(sd(diff(T))>1e-10) stop("Sorry - time intervals must be constant to use this method.")
  
  lag <- T[1:min(300,length(T)/2)]
  V <- diff(Z)/diff(T)
  
  if(spline)
  {
    VZT <- getV.spline(Z,T)
    V <- VZT$V
    T <- VZT$T
    Z <- VZT$Z
  }
  
  lag.scalar <- lag[-1]/dT  
  vaf <- apply(as.matrix(lag.scalar),1,getVaf, V=V)
  vaf <- c(1,as.vector(vaf[-length(vaf)]))
  lag <- lag[-length(lag)]
  
  truncate <- 30
  if(min(vaf) < exp(-3))
  {
    truncate <- min(which(vaf<exp(-3)))
    vaf <- vaf[1:truncate]
    lag <- lag[1:truncate]
  }
  
  if(truncate < 3 | truncate == "Inf"){ 
    warning("Data are too sparcely sampled for any estimate of tau.")
    beta.hat <- NA; beta.CI <- NA 
  } else if(truncate < 10){ 
    warning("Data are too sparcely sampled for robust estimate of tau.")
    beta.hat <- lm(log(vaf)~lag-1)$coef; beta.CI <- NA
  } else {
    tau.gls <- try(gls(log(vaf[vaf > 0])~lag[vaf > 0]-1, correlation = corAR1(form=~1), weights=varExp()), silent=TRUE)
    if(is(tau.gls)!="try-error")
    {
      beta.hat <- tau.gls$coef
      beta.CI <- intervals(tau.gls)[[1]][c(1,3)]
    } else {
      tau.lm <- lm(log(vaf[vaf>0])~lag[vaf>0]-1)
      beta.hat <- tau.lm$coef
      beta.CI <- beta.hat + c(-1,1) *qt(0.975, df=length(vaf[vaf > 0])-1)*summary(tau.lm)$coef[1,2]
      warning("Generalized least squares fit failed - confidence intervals are probably suspect.")
    }}
    
    tau.hat <- as.numeric(-1/beta.hat)
    nu.hat <- mean(Mod(V))
    results <- data.frame(tau.hat = tau.hat, nu.hat = nu.hat)
    
    if(CI)
    {
      # nu estimates
      rho.hat <- cor(Mod(V[-1]), Mod(V[-length(V)]))
      nu.se <- sd(Mod(V))/sqrt(length(V))/(1-rho.hat)/2
      nu.CI <- nu.hat +  c(-1,1) * qnorm(0.975) * nu.se
      
      # tau estimates
      tau.CI <- -1/beta.CI
      
      # compile results
      results <- data.frame(t(results), rbind(tau.CI, nu.CI))
      row.names(results) <- c("tau", "nu")
      names(results) <- c("Estimate", "C.I.low", "C.I.high")
    }
    
    if(diagnose)
    {   
      plot(lag, log(vaf), ylab="log(vaf)", main="log(vaf) should look linear over this range", bty="l", cex.lab=1.25, 
           cex.sub=1.25, font.sub = 2)
      abline(0, beta.hat, col=2)
    }
    return(results)
  }
  