PlotSegments <- function(Segment.prep, nK = NULL, cp.col="orange", mean.color = "red", cp.lwd=1, ...)
{  
  Data.traj <- Segment.prep$Data.traj
  models <- Segment.prep$models
  if(is.null(nK)) nK <- Segment.prep$nK
  
  # partition the parameters
  
  pm <- partmod.ltraj(Data.traj, nK, models)
  cp1 <- unlist(lapply(pm$ltraj, function(X) X$date[1]))
  cp2 <- unlist(lapply(pm$ltraj, function(X) X$date[length(X$date)]))
  cps <- cp1[-1]
  
  mus <- Segment.prep$mus[pm[[2]]$mod]
  if(Segment.prep$log) mus <- exp(mus)
  
  plot(Data.traj[[1]]$date, Data.traj[[1]]$dist, ...)
  lines(Data.traj[[1]]$date, Data.traj[[1]]$dist, col="darkgrey")
  segments(cp1, mus, cp2, mus, col=mean.color, lwd=2)
  abline(v = cps, col=cp.col, lwd=cp.lwd)
  return(pm)
}		