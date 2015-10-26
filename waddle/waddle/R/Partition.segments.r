##' Perform BPMM partitioning
##' 
##' This function partitions a movement time series in the given number of partitions.  
##' 
##' @param Segment.prep output of the Prep.segments function (a list containing the Data, the number of parameters, the candidate means)
##' @param nK number of partitions.  If NULL, uses the number suggested by the \code{\link{Prep.segments}} function. 
##' @param ... Values to be passed to the plotting function.
##' @return No value is returned.  A plot is produced with the time series and vertical changepoint segments. 

Partition.segments <- function(Segment.prep, nK = NULL)
{  
  Data.traj <- Segment.prep$Data.traj
  models <- Segment.prep$models
  mus <- Segment.prep$mus
  if(is.null(nK)) nK <- Segment.prep$nK
  
  # partition the parameters
  
  pm <- partmod.ltraj(Data.traj, nK, models)
  
  Model.bpm <- as.numeric(substr(pm[2]$stats$which.mod,5,10))
  if(Segment.prep$log) Mus.bpm <- round(exp(mus[Model.bpm]),2) else Mus.bpm <- mus[Model.bpm]
  Phases.bpm <- data.frame(Model = Model.bpm, Mu = Mus.bpm, summary(pm$ltraj))
  return(list(pm = pm, Phases.bpm = Phases.bpm, Segment.prep = Segment.prep))
}
