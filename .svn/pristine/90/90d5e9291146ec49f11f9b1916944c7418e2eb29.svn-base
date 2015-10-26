##' Parameter summary
##' 
##' Returns the parameter mean and standard error for the Weibull and wrapped Cauchy distributions in the mixed random walk model. 
##' Parameters not present in a model are returned as NA.
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @return A vector of model parameter values 
##' @author Chloe Bracis
getParameterValues <- function(jagsResults)
{
  params <- c("a[1]", "a[2]", "a[3]", "b[1]", "b[2]", "b[3]", "mu[1]", "mu[2]", "mu[3]", "rho[1]", "rho[2]", "rho[3]")
  values <- NULL
  
  for (p in params)
  {
    pMean <- round( getParamMean(jagsResults, p), digits = 1 )
    pSD <- round( getParamSD(jagsResults, p), digits = 1 )
    # return NA instead of NaN for missing params
    new <- if (is.nan(pMean)) { NA } else { paste0(pMean, " (", pSD, ")") }
    values <- rbind(values, new)
  }
  row.names(values) <- params
  return(values)
}
