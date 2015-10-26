.getMcmcDropIdx<- function(jagsResults)
{
  mcmcChains <- as.mcmc(jagsResults)
  
  # don't use idx parameter (state assignment of each data point) in convergence analysis
  if ("idx" %in% substr(varnames(mcmcChains), 1, 3))
  {
    mcmcChains <- mcmcChains[,-which(substr(varnames(mcmcChains), 1, 3) == "idx")]
  }
  return(mcmcChains)
}
