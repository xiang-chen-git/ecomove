##' Plot results of multi-state random walk analysis
##' 
##' Plots the results of the analysis compiled by the \code{CompileResults.mrw} function, derived from those results saved in the \code{mrw} package
##' 
##' @param Results a results data.frame, outputted from \link{CompileResults.mrw}
##' @param cols colors of states 1, 2 and 3 (default dark grey, light grey and white)
##' @param plotv whether or not to plot the velocity on top of the figure.
##' @param v.col color of the response variable (velocity)
##' @param y.labels labels of the three models. Most descriptive would be something like: \code{c("Two-State", "Two-State \n Switching Model", "Three-State \n Switching Model")}
##' @param v.lab whether or not to label the right y-axis.
##' @param x.axis whether or not to label the x axis.
##' @param ... further parameters to pass to the plotting of the velocity function
##' @seealso \code{\link{CompileResults.mrw}}

plot.mrw <- function(Results, cols = c("white", "grey20", "grey50"), plotv = TRUE, v.col="red", y.labels = c("A", "B", "C"), v.lab=TRUE, x.axis=TRUE, xlim = range(Results$Time), ...)
{
  if(!("mrw" %in% class(Results)))
    stop(call = "So sorry, this is the wrong kind of result.  Be sure to feed me the output of the 'CompileResults.mrw()' function.")
  
  Results.matrix <- as.matrix(cbind(Results$TripleSwitch, Results$DoubleSwitch, Results$Double))
  
  image(x = as.numeric(Results$Time), y = 1:3,
        z = Results.matrix, col=cols, xaxt="n", yaxt="n", xlab="", ylab="Model", xaxs="r", yaxs="r", xlim=xlim)
  abline(h = c(1.5,2.5), col="white")

  axis(2, at = 3:1, las=2, labels = y.labels)
  
  if(x.axis)
  {
    par(new=TRUE)
    plot(Results$Time, Results$V, type="n", xlab="", ylab="", yaxt="n", bty="n", xlim=xlim)
  }
  
  par(new=TRUE)
  # Velocity
  if(plotv){
  V <- Results$V
  plot(Results$Time, V, axes=FALSE, type="o", col=v.col, pch=21, xlim=xlim, ...)}

  if(v.lab)
  { 
    par(col.axis=v.col)
    axis(4, las=2, col=v.col)
    text(max(T)+ 2000, max(V)*1.2, label = "Velocity",col=2, xpd=TRUE, font=4, cex=1.25)
    text(min(T) - 10000, max(V)*1.2, label = "Model", xpd=TRUE, font=4, cex=1.25)
    par(col.axis="black")
  }
}