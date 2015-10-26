##' Preliminary segmentation analysis
##' 
##' Takes a trajectory and determines the number of partitions of a trajectory based on 
##' Markov models. The response variable here is limited to the step lengths.  This is a 
##' wrapper for the partitioning tools in \code{adehabitatLT} by Calenge.
##' 
##' @param Data.traj trajectory
##' @param sd standard deviation of step response
##' @param Km the maximum number of partitions of the trajectory
##' @param plotit whether to plot the likelihood analysis
##' @param nmodels number of candidate models 
##' @param log Whether to perform the analysi on the log of the step lengths. 
##' 
##' @return a list with the following elements: \item{Data.traj}{a regularized trajectory} \item{nK}{the optimal number of partitions} \item{mus}{the mean values of all the candidate models} \item{models}{the index of the selected models}
##' 
##' @seealso \code{\link{modpartltraj}}
##' 
Prep.segments <- function(Data.traj, sd=NULL, 
                         Km = 30, plotit = TRUE,
                         nmodels = 10, log = FALSE, mumin = 0, ...)
{  
  if(!attr(Data.traj, "regular"))
    Data.traj <- RegularizeTraj(Data.traj, ...)
  
  # create list of proposed models
  mu.max <-  max(Data.traj[[1]]$dist, na.rm=TRUE) 
  if(log) mu.max <- log(mu.max)
  mus <- seq(mumin, mu.max, length = nmodels)
  
  if(!log) 
  {
    if(is.null(sd)) sd <- sd(Data.traj[[1]]$dist, na.rm=TRUE)/nmodels
    model.list <- as.list(paste("dnorm(dist, mean =", mus, ", sd =", sd, ")", sep=""))
  }	else
  {
    if(is.null(sd)) sd <- sd(log(Data.traj[[1]]$dist), na.rm=TRUE)/nmodels
    model.list <- as.list(paste("dnorm(log(dist), mean =", mus, ", sd =", sd, ")", sep=""))
  }
  
  models <- modpartltraj(Data.traj, model.list)
  
  # calculate number of partitions
  bp <- bestpartmod(models, Km, plotit, correction=TRUE)
  nK <-  which.max(apply(bp$correction, 2, median, na.rm=TRUE))
  
  return(list(Data.traj = Data.traj, nK = nK, mus = mus, models = models, log = log, sd=sd))
}