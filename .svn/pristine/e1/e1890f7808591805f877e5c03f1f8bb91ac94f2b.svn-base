##' Smooth Track
##' 
##' Smooths track with a moving average
##' 
##' @details This function is robust to a mixture of real and complex numbers, and will handle date-time columns as long as they are called \code{Time}.
##' 
##' @param Track a movement track (designed for the \code{track} class, but will work for any numeric matrix)
##' @param steps The number of steps to roll the average over.  \code{steps}=1 returns the original data, \code{steps}
##' @examples data(Lamprey)
##' par(mfrow=c(2,2))
##' plot(Lamprey, main="No Smooth")
##' plot(SmoothTrack(Lamprey, 3), main="3 step smooth")
##' plot(SmoothTrack(Lamprey, 5), main="5 step smooth")
##' plot(SmoothTrack(Lamprey, 10), main="10 step smooth")
##' 
##' @author Eli Gurarie
##' @title Smooth Track

SmoothTrack <- function(Track, steps=1)
{
  Smooth <- function(X, steps) 
  {
    X.stack <- matrix(NA, nrow=length(X)-steps, ncol=steps)
    for(i in 1:steps)
      X.stack[,i] <- X[i:(length(X) - steps - 1 + i)]
    rowMeans(X.stack)
  }
  Track$Time <- as.double(Track$Time)
  tz <- attr(Track$Time, "tzone")
  Track.smoothed <- as.data.frame(apply(Track,2,Smooth, steps=steps))
  
  which.Re <- which(colSums(apply(Track.smoothed,2,Im)) == 0)
  
  Track.smoothed[,which.Re ]  <- apply(Track.smoothed[,which.Re],2,Re)
  
  Track.smoothed$Time <- as.POSIXct(Track.smoothed$Time, origin = "1970-01-01 00:00.00 UTC", tz=tz)
  class(Track.smoothed) <- class(Lamprey)
  return(Track.smoothed)
}




