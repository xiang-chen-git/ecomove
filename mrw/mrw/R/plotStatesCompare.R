##' Plot states 
##' 
##' Plots assigned state for each step for a MRW model, comparing different methods of determining state for each step across iterations
##' (mean, median, mode, iteration most similar to mode). Note that for 3 state models, the mean and median can be problematic.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result 
##' @param nstates The number of states in the model (either 2 or 3)
##' @seealso \code{\link{getMeanStates}}, \code{\link{getMedianStates}},, \code{\link{getModeStates}}, \code{\link{getIterationSimilarModeStates}}, \code{\link{plotStateProbabilities}}
##' 
##' @author Chloe Bracis


plotStatesCompare <- function(jagsResults, nstates = 2)
{
	col = if(nstates == 2) { c("red", "blue") } else { c("red", "blue", "yellow") }
	colors = colorRampPalette(col)(24)
	values = cbind(getMeanStates(jagsResults), getMedianStates(jagsResults), getModeStates(jagsResults), getIterationSimilarModeStates(jagsResults)) 
	image(x = 1:nrow(values), y = 1:ncol(values), z = values, col = colors, xlab = "Time", ylab = "State", yaxt = "n")
	axis(2, at = 1:4, labels = c("mean", "median", "mode", "chain"))
	legend(grconvertX(0.5,'nfc'), grconvertY(0.9,'nfc'), xjust = 0.5, yjust = 0, 
         legend = paste("state", 1:length(col)), fill = col, bty = "n", horiz = TRUE, xpd = TRUE)
}
