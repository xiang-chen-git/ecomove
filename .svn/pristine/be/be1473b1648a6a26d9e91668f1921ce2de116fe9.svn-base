rm(list=ls())
require(waddle)
require(rjags)
require(R2jags)

# subsample wolf to daily time step

data(Wolf)
Wolf2 <- data.frame(X = tapply(Wolf$X, substr(Wolf$Time, 1, 10), mean),
										Y = tapply(Wolf$Y, substr(Wolf$Time, 1, 10), mean))
Wolf2$Time <- as.POSIXct(row.names(Wolf2))

Wolf.Data <- Wolf2
Wolf.VT <- GetVT(Wolf.Data, units = "day")

# Mixed random walk model fitting

Wolf.MRW <- list("numT" = nrow(Wolf.VT), "step" = Wolf.VT$S, "theta" = Wolf.VT$Theta)

# single state model
initsSingleState <- function() { list(a = runif(1, 0, 10), 
									  b = runif(1, 0, 10), 
									  mu = runif(1, -3*pi/2, pi/2), 
									  rho = runif(1, 0, 1))}
parametersSingleState <- c("a", "b", "mu", "rho")
system.time(Wolf.singleState <- jags(Wolf.MRW, initsSingleState, parametersSingleState, "SingleState.txt", 
						n.chains = 2, n.iter = 50000, n.thin = 25, n.burnin = 10000))
analyzeConvergence(Wolf.singleState, plotTraces = TRUE)


# double state model
initsDoubleState = function() { list(a = sort(runif(2, 0.5, 2.5)),
									 b = sort(runif(2, 0, 5)),
									 mu = runif(2, -3*pi/2, pi/2),
									 rho = sort(runif(2, 0, 1)) ) }
parametersDoubleState = c("a", "b", "mu", "rho", "idx")
system.time(Wolf.doubleState <- jags(Wolf.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
																n.chains = 2, n.iter = 100000, n.thin = 50, n.burnin = 10000))
analyzeConvergence(Wolf.doubleState, plotTraces = TRUE)

# double state switch model
initsDoubleStateSwitch <- function() {list (a = sort(runif(2, 0.5, 2.5)), 
											b = sort(runif(2, 0, 5)), 
											mu = runif(2, -3*pi/2, pi/2), 
											rho = sort(runif(2, 0, 1)),
											p = sort(runif(2, 0, 1), decreasing = TRUE))}
parametersDoubleStateSwitch <- c("a", "b", "mu", "rho", "p", "idx")
system.time(Wolf.doubleStateSwitch <- jags(Wolf.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
																			n.chains = 2, n.iter = 100000, n.thin = 50, n.burnin = 10000))
analyzeConvergence(Wolf.doubleStateSwitch, plotTraces = TRUE)

# triple state switch model
initsTripleStateSwitch <- function() {list (a = sort(runif(3, 0.5, 2.5)), 
																						b0 = sort(runif(3, 0, 5)), 
																						mu = runif(3, -3*pi/2, pi/2), 
																						rho = sort(runif(3, 0, 1)),
																						p = sort(runif(3, 0, 1), decreasing = TRUE),
																						phi = sort(runif(3, 0, 1), decreasing = TRUE))}
parametersTripleStateSwitch <- c("a", "b", "mu", "rho", "p", "phi", "idx")
system.time(Wolf.tripleStateSwitch <- jags(Wolf.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitchOrderedB.txt",
																			n.chains = 2, n.iter = 1000000, n.thin = 1000, n.burnin = 250000))
analyzeConvergence(Wolf.tripleStateSwitch)

# NOTE: this depends on the results of the analysis
# Here we choose to flip the states for the doubl state and double state switch models so that the meaning
# of the states is consistent across all models
Wolf.doubleState <- flipChainsDoubleModel(jagsResults = Wolf.doubleState, chainIndex = 1)
Wolf.doubleState <- flipChainsDoubleModel(jagsResults = Wolf.doubleState, chainIndex = 2)
Wolf.doubleStateSwitch <- flipChainsDoubleModel(jagsResults = Wolf.doubleStateSwitch, chainIndex = 1)
Wolf.doubleStateSwitch <- flipChainsDoubleModel(jagsResults = Wolf.doubleStateSwitch, chainIndex = 2)


# results of all analyses
save(Wolf.singleState, file="../../mrw/mrw/data/Wolf.singleState.rda", compress = TRUE)
save(Wolf.doubleState, file="../../mrw/mrw/data/Wolf.doubleState.rda", compress = TRUE)
save(Wolf.doubleStateSwitch, file="../../mrw/mrw/data/Wolf.doubleStateSwitch.rda", compress = TRUE)
save(Wolf.tripleStateSwitch, file="../../mrw/mrw/data/Wolf.tripleStateSwitch.rda", compress = TRUE)
save(Wolf.Data, file="../../mrw/mrw/data/Wolf.Data.rda", compress = TRUE)
save(Wolf.VT, file="../../mrw/mrw/data/Wolf.VT.rda", compress = TRUE)


