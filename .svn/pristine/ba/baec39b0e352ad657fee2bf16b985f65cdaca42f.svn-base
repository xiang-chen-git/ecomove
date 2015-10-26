PathPlot.segments <- function (Data.segments, n.dots = 6, lwd.segments = 1, where = "left", ncol=2, cex=2, palette = "topo.colors", ...) 
{
  pm <- Data.segments$pm
  Segment.prep <-  Data.segments$Segment.prep 
 
  mod <- pm$stat$mod
  mus <- Segment.prep$mus[pm$stat$mod]
  if(Segment.prep$log) mus <- exp(mus)
  nb.reloc <- sapply(pm$ltraj, nrow)
  mu.fit <- rep(mus, nb.reloc)
  mod.fit <- rep(mod, nb.reloc)
  
  eval(parse(text=paste("palette(",palette,"(max(mod)+1)",")")))
  
  cols <- max(mod) - mod.fit+1
  
  xy <- do.call("rbind", lapply(pm$ltraj, function(i) data.frame(i$x, i$y)))
  plot(xy, asp = 1, col="darkgrey", type="l", ...) 
 # points(xy[c(1,nrow(xy)),], pch=c(24, 23), bg=c("green", "red"), cex=2, lwd=1.5, col="darkgrey")
  points(xy, pch=21, bg=alpha(cols, 0.7), 
         col=adjustcolor(cols, offset = c(-.3,-.3,-.3,0)),
         cex=sqrt(cex*1/mu.fit + 0.5))
  
  mymodel <- round(seq(1, max(mod), length = n.dots))
  mylegend <- signif(Segment.prep$mus[mymodel],2)
  legend.cols <-  max(mod) - mymodel +1
  
  if(Segment.prep$log) mylegend <- signif(exp(Segment.prep$mus[mymodel]),2)
  
  legend(where, pch=21, col= "darkgrey", pt.bg=alpha(legend.cols, 0.7), title="mean speed", 
         legend=mylegend, bty="n", ncol=ncol, pt.cex=sqrt(cex*1/mylegend + 0.5))
}
