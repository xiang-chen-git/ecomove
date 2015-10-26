##' Plot Segments
##' 
##' This function plots the time series and change points for the BPMM output.
##' 
##' @param Partition This is a list containing the partitioned model (pm), and the output of the PrepSegments function.
##' @param {mean.col} color of horizonal mean segments
##' @param {cp.col,cp.lwd}  color and width of the vertical change point demarcations
##' @param ... additional parameters  to be passed to the plot function.
##' @return No value is returned.  A plot is produced with the time series and vertical changepoint segments. 

plot.segments <- function(Partition, mean.col = "red", cp.col="orange", cp.lwd=1, ...)
{
  
  pm <- Partition$pm
  Segment.prep <- Partition$Segment.prep
  Data.traj <- Segment.prep$Data.traj
  
  cp1 <- unlist(lapply(pm$ltraj, function(X) X$date[1]))
  cp2 <- unlist(lapply(pm$ltraj, function(X) X$date[length(X$date)]))
  cps <- cp1[-1]
  
  mus <- Segment.prep$mus[pm[[2]]$mod]
  if(Segment.prep$log) mus <- exp(mus)
  
  plot(Data.traj[[1]]$date, Data.traj[[1]]$dist, ...)
  lines(Data.traj[[1]]$date, Data.traj[[1]]$dist, col="darkgrey")
  segments(cp1, mus, cp2, mus, col=mean.col, lwd=2)
  abline(v = cps, col=cp.col, lwd=cp.lwd)
}  	