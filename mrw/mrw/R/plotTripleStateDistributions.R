##' Plot fitted model distributions
##' 
##' Plots the empirical and fitted distributions of steps and turning angles assigned to each state for a MRW model.
##' 
##' @param tripleState An \code{\link[=rjags-class]{rjags}} result for a MRW model with 3 states
##' @param step The vector of steps used to fit the model
##' @param theta The vector of turning angles used to fit the model
##' @seealso \code{\link{plotDoubleStateDistributions}}
##' 
##' @author Chloe Bracis

plotTripleStateDistributions <- function(tripleState, step, theta)
{
  par(mfrow = c(2,2))
  
  # get plot limits
  hist1 <- hist(step[getModeStates(tripleState) == 1], plot = FALSE)
  hist2 <- hist(step[getModeStates(tripleState) == 2], plot = FALSE)
  hist3 <- hist(step[getModeStates(tripleState) == 3], plot = FALSE)
  ymax <- max(c(hist1$density, hist2$density, hist3$density))

  hist(step[getModeStates(tripleState) == 1], xlim=range(step), ylim = c(0, ymax), col=rgb(1,0,0,0.5), freq = FALSE, 
       xlab = "Step", main = "Triple state switch")                     
  hist(step[getModeStates(tripleState) == 2], col=rgb(0,0,1,0.5), freq = FALSE, add = TRUE)                     
  hist(step[getModeStates(tripleState) == 3], col=rgb(1,1,0,0.5), freq = FALSE, add = TRUE)                     
  curve(dweibull(x, getParamMean(tripleState, "a[1]"), getParamMean(tripleState, "b[1]")), 0.001, max(step), col = "red", lwd = 2, add = TRUE)
  curve(dweibull(x, getParamMean(tripleState, "a[2]"), getParamMean(tripleState, "b[2]")), 0.001, max(step), col = "blue", lwd = 2, add = TRUE)
  curve(dweibull(x, getParamMean(tripleState, "a[3]"), getParamMean(tripleState, "b[3]")), 0.001, max(step), col = "yellow", lwd = 2, add = TRUE)
  
  rose.diag(theta[getModeStates(tripleState) == 1], bins = 36, prop = 2, main = "State 1")
  rose.diag(theta[getModeStates(tripleState) == 2], bins = 36, prop = 2, main = "State 2")
  rose.diag(theta[getModeStates(tripleState) == 3], bins = 36, prop = 1, main = "State 3")
}
