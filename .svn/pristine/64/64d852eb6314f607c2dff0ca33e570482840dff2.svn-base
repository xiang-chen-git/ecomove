##' Plot multipath
##' 
##' Default method for plotting multipaths
##' 
##' @param multipath a multipath class object (minimally containing a vector Z of locations and a Phase element which enumerates the phases of the plots).
##' @param {pch,cex}  character type and size - defaults to 1/2 size (Cex = 0.5) filled circles (pch=19)
##' @param {cols}  colors of the distinct phases.  Defaults to the well contrasted rich.colors() palette from gplots. 
##' @param ... additional arguments passed to plot function
##' 
##' @source plot.multiplot.r

plot.multipath <- function(multipath, cols=NULL,  ...)
{
  Z <- multipath$Z
  Phase <- multipath$Phase
  if(is.null(cols))  cols <- rainbow(length(unique(Phase)), alpha=0.5)
  
  plot(Z, asp=1, pch=19, cex=0.5, col=cols[Phase], ...)
  segments(Re(Z[-length(Z)]), Im(Z[-length(Z)]), Re(Z[-1]), Im(Z[-1]), col=cols[Phase[-1]])
  points(Z[1], bg = "green", pch = 21)
  points(Z[length(Z)], bg = "red", pch = 23)
}