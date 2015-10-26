##' Flip parameter indices in MCMC chains 
##' 
##' This method takes an \code{\link[=rjags-class]{rjags}} object and flips the parameter indicies, then returns it. This is useful if the states are reversed from one chain to the next, 
##' i.e. state 1 is the more active state in one chanin but the more sedentatry state in another. However, the paramters and idx state variables are only
##' flipped in the \code{sims.array} part of the \code{\link[=rjags-class]{rjags}} object, so be careful not to use the calculated paramter statistics or other chain representations.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @param chainIndex Which chain within the \code{jagsResults} to flip
##' @return An \code{\link[=rjags-class]{rjags}} object with the a, b, mu, rho, and (optionally) p parameter indicies 1 and 2 flipped for
##' the specified chain. The parameters, as well as the idx state are flipped in the \code{sims.array} array, but not
##' in the \code{sims.matrix} or \code{sims.list}. Also, the calculated paramter means etc. in the \code{BUGSoutput} 
##' are not updated.
##' @seealso \link{flipChainsTripleModel}
##' @author Chloe Bracis


flipChainsDoubleModel <-
function(jagsResults, chainIndex)
{
  paramNames <- dimnames(jagsResults$BUGSoutput$sims.array)[[3]]
  
  # a
  a1 <- which(paramNames == "a[1]")
  a2 <- which(paramNames == "a[2]")
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, a1]
  jagsResults$BUGSoutput$sims.array[, chainIndex, a1] <- jagsResults$BUGSoutput$sims.array[, chainIndex, a2]
  jagsResults$BUGSoutput$sims.array[, chainIndex, a2] <- temp
  
  # b
  b1 <- which(paramNames == "b[1]")
  b2 <- which(paramNames == "b[2]")
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, b1]
  jagsResults$BUGSoutput$sims.array[, chainIndex, b1] <- jagsResults$BUGSoutput$sims.array[, chainIndex, b2]
  jagsResults$BUGSoutput$sims.array[, chainIndex, b2] <- temp
  
  # mu
  mu1 <- which(paramNames == "mu[1]")
  mu2 <- which(paramNames == "mu[2]")
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, mu1]
  jagsResults$BUGSoutput$sims.array[, chainIndex, mu1] <- jagsResults$BUGSoutput$sims.array[, chainIndex, mu2]
  jagsResults$BUGSoutput$sims.array[, chainIndex, mu2] <- temp
  
  # rho
  rho1 <- which(paramNames == "rho[1]")
  rho2 <- which(paramNames == "rho[2]")
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, rho1]
  jagsResults$BUGSoutput$sims.array[, chainIndex, rho1] <- jagsResults$BUGSoutput$sims.array[, chainIndex, rho2]
  jagsResults$BUGSoutput$sims.array[, chainIndex, rho2] <- temp
  
  # p (only in switching model)
  p1 <- which(paramNames == "p[1]")
  p2 <- which(paramNames == "p[2]")
  if (length(p1) == 1)
  {
    temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, p1]
    # need 1-p since we are fitting p11 and p21 (1st column of transition matrix)
    jagsResults$BUGSoutput$sims.array[, chainIndex, p1] <- 1 - jagsResults$BUGSoutput$sims.array[, chainIndex, p2]
    jagsResults$BUGSoutput$sims.array[, chainIndex, p2] <- 1 - temp
  }
  
  # idx switch states (swap 1's and 2's)
  idx <- which(substr(paramNames, 1, 3) == "idx")
  jagsResults$BUGSoutput$sims.array[, chainIndex, idx] <- mapvalues(jagsResults$BUGSoutput$sims.array[, chainIndex, idx], 
                                                                    from = 1:2, to = 2:1)
  
  return(jagsResults)
}
