PathPlot.fpt <- function (Data.traj, r = 20, where="topright", 
                          n.dots = 2, scale=1/4, k=2, unit="day", palette = "topo.colors", ...) 
{
  Data.fpt <- fpt(Data.traj, r, unit=unit)[[1]]
  x <- Data.traj[[1]]$x
  y <- Data.traj[[1]]$y
  
  plot(x, y, col="grey", type="l", asp=1, xlab="", ylab="", axes=FALSE, bty="n")
  
  fpt <- Data.fpt[,1]
  fpt.scaled <- fpt/max(fpt, na.rm=TRUE)
  fpt.integer <- round(fpt.scaled*1000)
  eval(parse(text=paste("palette(",palette,"(1024)",")")))
  points(x,y, pch=21, bg=alpha(fpt.integer, 0.7), 
         col=adjustcolor(fpt.integer, offset = c(-.3,-.3,-.3,0)), 
         cex=(fpt.scaled)^(scale)*k)
  
  legend(where, 
         pt.cex=seq(min(fpt.scaled, na.rm=TRUE), 1, length=n.dots)^(scale)*k, 
         pt.bg=round(seq(min(fpt.scaled, na.rm=TRUE), 1, length=n.dots)*1000), 
         col="darkgrey",
         pch=21, 
         legend=round(seq(min(fpt, na.rm=TRUE), max(fpt, na.rm=TRUE), length=n.dots)), 
         title="FPT", bty="n", ...)
  palette("default")
}