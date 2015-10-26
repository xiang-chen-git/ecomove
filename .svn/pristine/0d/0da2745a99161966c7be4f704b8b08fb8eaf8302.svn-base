##' Subsample or interpolate movement points
##'
##' This is just a simplified wrapper for adehabitatLT's \code{\link[adehabitatLT]{redisltraj}}.
##'
##' @param Data a data frame containing (at least) an "X", a "Y" and a "Time"
##' @param n time interval of interpolation / subsampling
##' @param units one of \code{"sec", "min", "hour", "day"}
##' @return list with two elements: \item{Data}{The data frame, regularized - as a \code{track} class object.} \item{N.interpolated}{The number of points added to the data frame (via interpolation) or removed from the data frame (filtering)}
##' @author Eli Gurarie
##' @examples 
##' data(Lamprey)
##' L0.5 <- InterpolatePoints(Lamprey, 30); L0.5$N.interpolated
##' L1 <- InterpolatePoints(Lamprey, 60); L1$N.interpolated
##' L3 <- InterpolatePoints(Lamprey, 180); L3$N.interpolated
##' 
##' plot(L1$Data$Z[1:9], type="o", cex=1.5, asp=1);
##' points(L3$Data$Z, col=2, type="o", pch=19)
##' points(L0.5$Data$Z, col=3, pch=19, cex=0.5, type="o")
##' legend("topright", col=1:3, pch=c(1,19,19), legend=c("1 minute", "3 minutes", "0.5 minutes"), lwd=1)

InterpolatePoints <- function(Data, n = 60, units=c("sec", "min", "hour", "day")[1], id="id")
{
  k <- ifelse(units == "sec", 1, 
              ifelse(units == "min", 60, 
                     ifelse(units == "hour", 60*60, 
                            ifelse(units == "day", 60*60*24, 
                                   stop("Invalid time unit.")))))
  nsec <- k*n
  
  Data.traj <- as.ltraj(data.frame(Data$X, Data$Y), as.POSIXct(Data$Time), id = id)
  Data.traj2 <- redisltraj(na.omit(Data.traj[1]), nsec, type="time")[1][[1]]
  
  Data2 <- data.frame(Time = Data.traj2$date,
                      X = Data.traj2$x, 
                      Y = Data.traj2$y,
                      Z = Data.traj2$x + 1i*Data.traj2$y)
  
  Data.interpolated <- Data[match(Data2$Time, Data$Time),]
  Data.interpolated$X <- Data2$X
  Data.interpolated$Y <- Data2$Y
  Data.interpolated$Z <- Data2$Z
  Data.interpolated$Time <- Data2$Time
  row.names(Data.interpolated) <- 1:nrow(Data.interpolated)
  class(Data.interpolated) <- c("track", "data.frame")
  return(list(Data = Data.interpolated, N.interpolated = nrow(Data2) - nrow(Data)))
}

##' @title Interpolating points 