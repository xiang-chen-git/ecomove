##' DIC and pD
##' 
##' Obtain the DIC and estimated number of parameters (pD) for an \code{\link[=rjags-class]{rjags}} fit.
##' 
##' 
##' @param jagsResults An \code{\link[=rjags-class]{rjags}} result
##' @return A data frame with the DIC and pD
##' @author Chloe Bracis

getDIC <- function(jagsResults)
{
	DIC <- sapply(jagsResults, function(x) x$BUGSoutput$DIC)
	pD <- sapply(jagsResults, function(x) x$BUGSoutput$pD)
	results <- data.frame(DIC, pD, row.names = sapply(jagsResults, function(x) x$BUGSoutput$model.file))
	return(results)
}
