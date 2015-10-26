##' Flip parameter indices in MCMC chains 
##' 
##' This method takes an \code{\link[=rjags-class]{rjags}} object and flips the parameter indicies, then returns it. This is useful if the states are reversed from one chain to the next, 
##' i.e. state 1 is the more active state in one chanin but the more sedentatry state in another. However, the paramters and idx state variables are only
##' flipped in the \code{sims.array} part of the \code{\link[=rjags-class]{rjags}} object, so be careful not to use the calculated paramter statistics or other chain representations.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @param chainIndex Which chain within the \code{jagsResults} to flip
##' @param idxA Which parameter index (1, 2, or 3) to flip
##' @param idxB Which parameter index (1, 2, or 3) to flip
##' @return An rjags object with the specified a, b, mu, rho, p, and phi parameter indicies flipped for
##' the specified chain. The parameters, as well as the idx state are flipped in the \code{sims.array} array, but not
##' in the \code{sims.matrix} or \code{sims.list}. Also, the calculated paramter means etc. in the \code{BUGSoutput} 
##' ##' are not updated.
##' @seealso \link{flipChainsDoubleModel}
##' @author Chloe Bracis

flipChainsTripleModel <- function(jagsResults, chainIndex, idxA, idxB)
{
  paramNames <- dimnames(jagsResults$BUGSoutput$sims.array)[[3]]
  theA <- paste0("[", idxA, "]")
  theB <- paste0("[", idxB, "]")
  theC <- paste0("[", setdiff(1:3, c(idxA, idxB)), "]") # the chain not swapping
  
  # a
  aA <- which(paramNames == paste0("a", theA))
  aB <- which(paramNames == paste0("a", theB))
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, aA]
  jagsResults$BUGSoutput$sims.array[, chainIndex, aA] <- jagsResults$BUGSoutput$sims.array[, chainIndex, aB]
  jagsResults$BUGSoutput$sims.array[, chainIndex, aB] <- temp
  
  # b
  bA <- which(paramNames == paste0("b", theA))
  bB <- which(paramNames == paste0("b", theB))
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, bA]
  jagsResults$BUGSoutput$sims.array[, chainIndex, bA] <- jagsResults$BUGSoutput$sims.array[, chainIndex, bB]
  jagsResults$BUGSoutput$sims.array[, chainIndex, bB] <- temp
  
  # mu
  muA <- which(paramNames == paste0("mu", theA))
  muB <- which(paramNames == paste0("mu", theB))
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, muA]
  jagsResults$BUGSoutput$sims.array[, chainIndex, muA] <- jagsResults$BUGSoutput$sims.array[, chainIndex, muB]
  jagsResults$BUGSoutput$sims.array[, chainIndex, muB] <- temp
  
  # rho
  rhoA <- which(paramNames == paste0("rho", theA))
  rhoB <- which(paramNames == paste0("rho", theB))
  temp <- jagsResults$BUGSoutput$sims.array[, chainIndex, rhoA]
  jagsResults$BUGSoutput$sims.array[, chainIndex, rhoA] <- jagsResults$BUGSoutput$sims.array[, chainIndex, rhoB]
  jagsResults$BUGSoutput$sims.array[, chainIndex, rhoB] <- temp
  
  # 3x3 transition matrix, which is a function of p and phi
  pA <- which(paramNames == paste0("p", theA))
  pB <- which(paramNames == paste0("p", theB))
  pC <- which(paramNames == paste0("p", theC))
  phiA <- which(paramNames == paste0("phi", theA))
  phiB <- which(paramNames == paste0("phi", theB))
  phiC <- which(paramNames == paste0("phi", theC))
  
  theChain <- jagsResults$BUGSoutput$sims.array[, chainIndex, ]
  if (idxA + idxB == 3) #flip 1 and 2
  {
    pAnew <- (1 - theChain[,pB]) * theChain[,phiB]
    pBnew <- (1 - theChain[,pA]) * theChain[,phiA]
    pCnew <- (1 - theChain[,pC]) * theChain[,phiC]
    phiAnew <- theChain[,pB] / (1 - pAnew)
    phiBnew <- theChain[,pA] / (1 - pBnew)
    phiCnew <- theChain[,pC] / (1 - pCnew)
  }
  else if (idxA + idxB == 4) # flip 1 and 3
  {
    pAnew <- (1 - theChain[,pB]) * (1 - theChain[,phiB])
    pBnew <- (1 - theChain[,pA]) * (1 - theChain[,phiA])
    pCnew <- (1 - theChain[,pC]) * (1 - theChain[,phiC])
    phiAnew <- (1 - theChain[,pB]) * theChain[,phiB] / (1 - pAnew)
    phiBnew <- (1 - theChain[,pA]) * theChain[,phiA] / (1 - pBnew)
    phiCnew <- (1 - theChain[,pC]) * theChain[,phiC] / (1 - pCnew)
  }
  else if (idxA + idxB == 5) # flip 2 and 3
  {
    pAnew <- theChain[,pB]
    pBnew <- theChain[,pA]
    pCnew <- theChain[,pC]
    phiAnew <- (1 - theChain[,pB]) * (1 - theChain[,phiB]) / (1 - pAnew)
    phiBnew <- (1 - theChain[,pA]) * (1 - theChain[,phiA]) / (1 - pBnew)
    phiCnew <- (1 - theChain[,pC]) * (1 - theChain[,phiC]) / (1 - pCnew)		
  }
  else
  {
    stop(paste("Problem with specified parameter indicies", idxA, "and", idxB))
  }
  
  jagsResults$BUGSoutput$sims.array[, chainIndex, pA] <- pAnew
  jagsResults$BUGSoutput$sims.array[, chainIndex, pB] <- pBnew
  jagsResults$BUGSoutput$sims.array[, chainIndex, pC] <- pCnew
  jagsResults$BUGSoutput$sims.array[, chainIndex, phiA] <- phiAnew
  jagsResults$BUGSoutput$sims.array[, chainIndex, phiB] <- phiBnew
  jagsResults$BUGSoutput$sims.array[, chainIndex, phiC] <- phiCnew
  
  # idx switch states
  idx <- which(substr(paramNames, 1, 3) == "idx")
  jagsResults$BUGSoutput$sims.array[, chainIndex, idx] <- mapvalues(jagsResults$BUGSoutput$sims.array[, chainIndex, idx], 
                                                                    from = c(idxA, idxB), to = c(idxB, idxA))
  
  return(jagsResults)
}
