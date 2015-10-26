##' Analyze convergence of JAGS fit
##' 
##' Reports parameter means and standard deviations, as well as several convergence statistics: effective parameter
##' size, autocorrelation at lag 1, Gelman and Rubin's diagnostic, and Geweke's Z.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @param plotTraces \code{TRUE} to plot traces of parmater chains, \code{FALSE} to skip plotting
##' @param plotCumu \code{TRUE} to plot cumulative quanitiles of parmater chains, \code{FALSE} to skip plotting
##' 
##' @author Chloe Bracis

analyzeConvergence <- function(jagsResults, plotTraces = FALSE, plotCumu = FALSE)
{
  mcmcChains <- .getMcmcDropIdx(jagsResults)
  nchains <- nchain(mcmcChains)
  
  convergenceStats <- calculateConvergence(mcmcChains, nchains > 1)
  print(convergenceStats)
  if (plotTraces) { plot(mcmcChains) }
  if (plotCumu) { cumuplot(mcmcChains) }
}
