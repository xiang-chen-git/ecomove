##' Parameter mean
##' 
##' Returns the mean value of the parameter across chains and iterations. This is useful if the parameter indicies
##' have been flipped in a chain so that the calculated means in the \code{BUGSoutput} are not valid.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @param paramName The exact name of the parameter (matching those used in \code{BUGSoutput})
##' @return The mean value of the parameter across chains and iterations 
##' @author Chloe Bracis

getParamMean <- function(jagsResults, paramName)
{
  # Calculate directly from sims.array instead of using the param estimates in BUGSoutput 
  # in case states have been flipped in a chain
  paramIdx <- which(dimnames(jagsResults$BUGSoutput$sims.array)[[3]] == paramName)
  return( mean(jagsResults$BUGSoutput$sims.array[,,paramIdx]))
}
