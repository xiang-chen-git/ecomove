.getIdxArray <- function(jagsResults)
{
  if(class(jagsResults) != "rjags") stop("jagsResults parameter must be an rjags object")
  if(!("idx" %in% jagsResults$parameters.to.save)) stop("Must record idx parameter to get states")
  
  # must get all the state information from the idx in the sims.array in case states have been flipped
  return( jagsResults$BUGSoutput$sims.array[,,which(substr(dimnames(jagsResults$BUGSoutput$sims.array)[[3]], 1, 3) == "idx")] )
}
