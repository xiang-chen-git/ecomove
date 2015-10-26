##' Diagnostic Plot of BPMM output
##'
##' Residual diagnostic plots are a quick way to assess whether the residuals have a standard normal distribution and are independent are an appropriate model for the BPMM fit.  Also returns the residuals.
##'
##' @param Data.traj Trajectory class to analyze.
##' @param plot whether to produce the diagnostic plots.
##' @param echo whether to return the resisuals.
##' @return Returns the residuals of the model. Produces a qqnorm and histogram of the standardized model fit, with a standard normal distribution curve overlaid. 

DiagPlot.segments <- function(Partition, plot=TRUE, echo=FALSE, ...)
{
  Segment.prep <- Partition$Segment.prep
  Phases.bpm <- Partition$Phases.bpm
  sd <- Segment.prep$sd
  Data.traj <- Segment.prep$Data.traj
  log <- Segment.prep$log
  
  X <- Data.traj[[1]]$dist 
  if(log) X <- log(X)
  T <- Data.traj[[1]]$date
  X.hat <- rep(0, length(D))
    
  if(log) mus <- log(Phases.bpm$Mu) else mus <- Phases.bpm$Mu
  
  for (i in nrow(Phases.bpm):1) X.hat[T <= Phases.bpm$date.end[i]] <- mus[i]
  
  Z.hat <- (X - X.hat)/sd
  Z.hat <- Z.hat[Z.hat > -Inf]
  
  if(plot)
  {
    par(mfrow=c(1,3))
    qqnorm(Z.hat, ...); qqline(Z.hat)
    hist(Z.hat, freq=FALSE, col="grey", bor="darkgrey", breaks=20, ylim=c(0, dnorm(0)), ...)
    curve(dnorm(x,0,1), col="red", lwd=2, add=TRUE)
    acf(Z.hat[!is.na(Z.hat)])
  }
  if(echo) return(Z.hat)
}
