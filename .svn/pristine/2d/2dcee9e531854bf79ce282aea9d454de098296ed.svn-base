require(coda)

analyzeConvergence <- function(jagsResults, plotTraces = FALSE, plotCumu = FALSE)
{
	mcmcChains <- as.mcmc(jagsResults)
	nchains <- nchain(mcmcChains)
	
	# don't use idx parameter (state assignment of each data point) in convergence analysis
	if ("idx" %in% substr(varnames(mcmcChains), 1, 3))
	{
		mcmcChains <- mcmcChains[,-which(substr(varnames(mcmcChains), 1, 3) == "idx")]
	}

	convergenceStats <- calculateConvergence(mcmcChains, nchains > 1)
	print(convergenceStats)
	if (plotTraces) { plot(mcmcChains) }
	if (plotCumu) { cumuplot(mcmcChains) }
#	return(convergenceStats)
}

calculateConvergence = function(chain, multipleChains = FALSE)
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

flipChainsDoubleModel <- function(jagsResults, chainIndex)
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
	
	# idx switch states
	idx <- which(substr(paramNames, 1, 3) == "idx")
	# this will swap 1's and 2's
	jagsResults$BUGSoutput$sims.array[, chainIndex, idx] <- 3 - jagsResults$BUGSoutput$sims.array[, chainIndex, idx]
	
	return(jagsResults)
}

interpolatePoints <- function(Data, nsec = 60, id)
{
    Data.traj <- as.ltraj(data.frame(Data$X, Data$Y), as.POSIXct(Data$Time), id)
    Data.traj2 <- redisltraj(na.omit(Data.traj[1]), nsec, type="time")[1][[1]]
    
    Data2 <- data.frame(Time = Data.traj2$date,
                                                                                    X = Data.traj2$x,
                                                                                    Y = Data.traj2$y,
                                                                                    Z = Data.traj2$x + 1i*Data.traj2$y)
    
    Data.interpolated <- Data[match(Data2$Time, Data$Time),]
    Data.interpolated$X <- Data2$X
    Data.interpolated$Y <- Data2$Y
    Data.interpolated$Z <- Data2$Z
    Data.interpolated$Time <- Data2$Time
    row.names(Data.interpolated) <- 1:nrow(Data.interpolated)
    
    return(list(Data = Data.interpolated, N.interpolated = nrow(Data2) - nrow(Data)))
}

getDIC <- function(jagsResultsList)
{
	# TODO is this altered by tracking idx? Caluclate after the fact with coda instead?
	DIC <- sapply(jagsResultsList, function(x) x$BUGSoutput$DIC)
	pD <- sapply(jagsResultsList, function(x) x$BUGSoutput$pD)
	results <- data.frame(DIC, pD, row.names = sapply(jagsResultsList, function(x) x$BUGSoutput$model.file))
	return(results)
}

getMedianStates <- function(jagsResults)
{
	if(class(jagsResults) != "rjags") stop("jagsResults parameter must be an rjags object")
	if(!("idx" %in% jagsResults$parameters.to.save)) stop("Must record idx parameter to get states")
	
	return(jagsResults$BUGSoutput$median$idx)
}

getModeStates <- function(jagsResults)
{
	# best for 3 or more states
	probabilities <- getStateProbabilities(jagsResults)
	modes <- apply(probabilities, 1, which.max)
	return(modes)
}

getMeanStates <- function(jagsResults)
{
	if(class(jagsResults) != "rjags") stop("jagsResults parameter must be an rjags object")
	if(!("idx" %in% jagsResults$parameters.to.save)) stop("Must record idx parameter to get states")
	
	return(jagsResults$BUGSoutput$mean$idx)
}

getChainSimilarModeStates <- function(jagsResults)
{
	modes <- getModeStates(jagsResults)
	error <- apply(jagsResults$BUGSoutput$sims.list$idx, 1, function(x) sum((x - modes)^2))
	similarIdx <- which.min(error)
	return(jagsResults$BUGSoutput$sims.list$idx[similarIdx,])
}

getStateProbabilities <- function(jagsResults)
{
	if(class(jagsResults) != "rjags") stop("jagsResults parameter must be an rjags object")
	if(!("idx" %in% jagsResults$parameters.to.save)) stop("Must record idx parameter to get states")
	
	nstates <- length(unique(as.vector(jagsResults$BUGSoutput$sims.list$idx)))
	ndata <- ncol(jagsResults$BUGSoutput$sims.list$idx) 
	
	probabilities <- matrix(NA, nrow = ndata, ncol = nstates)
	
	for (i in 1:nstates)
	{
		probabilities[,i] <- apply(jagsResults$BUGSoutput$sims.list$idx, 2, function(x) sum(x == i))
	}
	probabilities <- probabilities / rowSums(probabilities)
	
	return(probabilities)
}

plotStates <- function(Z, state)
{
	plot(Z, col= rgb(((state-1) > 0.5),0,0,.2), pch=19, type="o")
	# plotStates(Data.VT$Z.start, doubleState$BUGSoutput$mean$idx)
	# rose.diag(Data.VT$Theta, bins = 36)
}

plotStateProbabilities <- function(jagsResults)
{
	probs <- getStateProbabilities(jagsResults)
	nsteps <- nrow(probs)
	nstates <- ncol(probs)

	plot(0, 0, type = "n", xlim = c(1, nsteps), xaxs = "i", yaxs = "i",
			 ylim = c(0, 1), xlab = "Time", ylab = "State probability", yaxt = "n")
	axis(2, at = c(0, 1))
	col = if(nstates == 2) { c("red", "blue") } else { c("red", "blue", "yellow") }
	
	xs <- c(1:nsteps, nsteps:1)
	bottom = rep(0, nsteps)
	
	for (i in 1:nstates)
	{
		top <- bottom + probs[,i]
		polygon(xs, c(bottom, rev(top)), col = col[i], border = NA)
		bottom <- top
	}
	legend(nsteps / 4, 1.15, legend = paste("state", 1:length(col)), fill = col, bty = "n", horiz = TRUE, xpd = TRUE)
}

plotStatesCompare <- function(jagsResults, nstates = 2)
{
	col = if(nstates == 2) { c("red", "blue") } else { c("red", "blue", "yellow") }
	colors = colorRampPalette(col)(24)
	values = cbind(getMeanStates(jagsResults), getMedianStates(jagsResults), getModeStates(jagsResults), getChainSimilarModeStates(jagsResults)) 
	image(x = 1:nrow(values), y = 1:ncol(values), z = values, col = colors, xlab = "Time", ylab = "State", yaxt = "n")
	axis(2, at = 1:4, labels = c("mean", "median", "mode", "chain"))
	legend(nrow(values) / 4, 4.9, legend = paste("state", 1:length(col)), fill = col, bty = "n", horiz = TRUE, xpd = TRUE)
	#legend("topright", legend = c("mean", "median", "mode", "chain"), pch = 1:4, lty = 1, bty = "n")
}