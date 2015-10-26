##' Plot state probabilities
##' 
##' Plots probability of each step being assigned to each state for a MRW model. 
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result 
##' @seealso \code{\link{getStateProbabilities}}, \code{\link{plotStatesCompare}}
##' 
##' @author Chloe Bracis

plotStateProbabilities <- function(jagsResults)
{
  probs <- getStateProbabilities(jagsResults)
  nsteps <- nrow(probs)
  nstates <- ncol(probs)
  
  plot(0, 0, type = "n", xlim = c(1, nsteps), xaxs = "i", yaxs = "i",
       ylim = c(0, 1), xlab = "Time", ylab = "State probability", yaxt = "n")
  axis(2, at = c(0, 1))
  col = if(nstates == 2) { c("red", "blue") } else { c("red", "blue", "yellow") }
  
  xs <- c(1:nsteps, nsteps:1)
  bottom = rep(0, nsteps)
  
  for (i in 1:nstates)
  {
    top <- bottom + probs[,i]
    polygon(xs, c(bottom, rev(top)), col = col[i], border = NA)
    bottom <- top
  }
  legend(grconvertX(0.5,'nfc'), grconvertY(0.9,'nfc'), xjust = 0.5, yjust = 0,
         legend = paste("state", 1:length(col)), fill = col, bty = "n", horiz = TRUE, xpd = TRUE)
}
