##' Calculate convergence statistics 
##' 
##' Returns parameter means and standard deviations, as well as several convergence statistics: effective parameter
##' size, autocorrelation at lag 1, Gelman and Rubin's diagnostic, and Geweke's Z.
##' 
##' @param chain A \code{\link{mcmc}} result
##' @param multipleChains \code{TRUE} if chain contains multiple chains, \code{FALSE} if it is a single chain
##' @return A data frame with parameter and convergence statistics
##' @author Chloe Bracis

calculateConvergence <- function(chain, multipleChains = FALSE)
{
	convergenceStatistics = data.frame(summary(chain)$statistics[, 1:2], summary(chain)$quantiles[,c(1,5)], row.names = varnames(chain))
	convergenceStatistics$effectiveSize = effectiveSize(chain)
	convergenceStatistics$acLag1 = as.vector(autocorr.diag(chain, lags = 1))
	
	if (multipleChains)
	{
		convergenceStatistics$gelman.diag = gelman.diag(chain)$psrf[,1]
		geweke = geweke.diag(chain)
		for (i in 1:length(geweke))
		{
			convergenceStatistics = cbind(convergenceStatistics, geweke[[i]]$z)
		}
	}
	return(convergenceStatistics)
}
