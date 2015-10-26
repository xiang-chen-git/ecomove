##' Plot fitted model distributions
##' 
##' Plots the empirical and fitted distributions of steps and turning angles assigned to each state for a MRW model.
##' 
##' @param doubleState An \code{\link[=rjags-class]{rjags}} result for a MRW model with 2 states
##' @param step The vector of steps used to fit the model
##' @param theta The vector of turning angles used to fit the model
##' @seealso \code{\link{plotTripleStateDistributions}}
##' 
##' @author Chloe Bracis

plotDoubleStateDistributions <- function(doubleState, step, theta)
{
  par(mfrow = c(1,3))
  
  # get plot limits
  hist1 <- hist(step[getModeStates(doubleState) == 1], plot = FALSE)
  hist2 <- hist(step[getModeStates(doubleState) == 2], plot = FALSE)
  ymax <- max(c(hist1$density, hist2$density))
  
  hist(step[getModeStates(doubleState) == 1], xlim=range(step), ylim = c(0, ymax), col=rgb(1,0,0,0.5), freq = FALSE, 
       xlab = "Step", main = "Double state")                     
  hist(step[getModeStates(doubleState) == 2], col=rgb(0,0,1,0.5), freq = FALSE, add = TRUE)                     
  curve(dweibull(x, getParamMean(doubleState, "a[1]"), getParamMean(doubleState, "b[1]")), 0.001, max(step), col = "red", lwd = 2, add = TRUE)
  curve(dweibull(x, getParamMean(doubleState, "a[2]"), getParamMean(doubleState, "b[2]")), 0.001, max(step), col = "blue", lwd = 2, add = TRUE)
  
  rose.diag(theta[getModeStates(doubleState) == 1], bins = 36, prop = 2, main = "State 1")
  
  rose.diag(theta[getModeStates(doubleState) == 2], bins = 36, prop = 2, main = "State 2")
}
