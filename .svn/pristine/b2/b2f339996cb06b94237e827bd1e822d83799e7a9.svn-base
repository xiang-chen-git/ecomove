##' Regularize Trajectory
##' 
##' A wrapper for adehabitat tools to make a trajectory regular.  Used within the 
##' 
##' 
##' @param Data.traj trajectory
##' @param dt interval for regularization
##' @param units can be "sec", "min", "hour", "day"
##' @return a regular traj object
##' @seealso \code{\link{PrepSegments}}

RegularizeTraj <- function(Data.traj, dt = 1, units = "hour")
{
  # Make the intervals "regular"
  refda <- as.POSIXct(Data.traj[[1]]$date[1])
  Data.traj <- setNA(Data.traj, refda, dt = dt, units = units)  
  Data.traj <- sett0(Data.traj, refda, dt = dt, units = units)
  return(Data.traj)
}