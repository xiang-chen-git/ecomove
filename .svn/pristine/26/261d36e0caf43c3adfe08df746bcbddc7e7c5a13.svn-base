##' Get transition matrix 
##' 
##' This method takes an \code{\link[=rjags-class]{rjags}} object and returns the transition matrix from the mean paramter values. The transition matrix 
##' is the matrix describing the probablity of switching from one state to another for hidden Markov models.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @param nstates The number of states in the model (either 2 or 3)
##' @return The transition matrix of probabilities \eqn{p_{ij}}{Pij} of switching states.
##' @author Chloe Bracis
##' 
##' 
getTransitionMatrix <- function(jagsResults, nstates = 2)
{
  # don't use parameter estimates from BUGSoutput in case states have been flipped in chains
  chains <- .getMcmcDropIdx(jagsResults)
  stats <- summary(chains)$statistics[,1] # 1st column is mean param value
  
  if (nstates == 2)
  {
    p11 <- stats[which(names(stats) == "p[1]")]
    p21 <- stats[which(names(stats) == "p[2]")]
    
    transMatrix <- matrix(c(p11, 1 - p11, p21, 1 - p21), byrow = TRUE, nrow = 2, ncol = 2)
  }
  else if (nstates == 3)
  {
    p1 <- stats[which(names(stats) == "p[1]")]
    p2 <- stats[which(names(stats) == "p[2]")]
    p3 <- stats[which(names(stats) == "p[3]")]
    phi1 <- stats[which(names(stats) == "phi[1]")]
    phi2 <- stats[which(names(stats) == "phi[2]")]
    phi3 <- stats[which(names(stats) == "phi[3]")]
    
    transMatrix <- matrix(c(p1, p2, p3, 
                            (1 - p1) * phi1, (1 - p2) * phi2, (1 - p3) * phi3,
                            (1 - p1) * (1 - phi1), (1 - p2) * (1 - phi2), (1 - p3) * (1 - phi3)), 
                          byrow = FALSE, nrow = 3, ncol = 3)
  }
  else
  {
    stop(paste(nstates, "is not a supported number of states"))
  }
  return(transMatrix)
}
