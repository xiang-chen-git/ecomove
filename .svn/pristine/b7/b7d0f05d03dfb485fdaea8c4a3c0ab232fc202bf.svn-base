##' Plot Track
##' 
##' Default method for plotting tracks
##' 
##' @param track track class object (minimally a dataframe or list containing an X and Y column)
##' @param {pch,col}  character type and color - defaults to transclucent filled grey circles. 
##' @param ... additional arguments passed to plot function
##' 
##' @source plot.track.r


plot.track <- function(track, pch=19, col =rgb(0,0,0,.2), ...)
{
  x <- track$X
  y <- track$Y
  plot(x, y, asp=1, type="o", pch=pch, cex=0.5, col=col, ...)
  points(x[1], y[1], bg="green", pch=21)
  points(x[length(x)], y[length(x)], bg="red", pch=23)  
}
