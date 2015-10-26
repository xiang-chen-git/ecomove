rm(list=ls())
require(waddle)
require(rjags)
require(R2jags)

data(Lamprey)

# Mixed random walk model fitting

Lamprey.Data <- InterpolatePoints(Lamprey, nsec = 120, id = "Lamprey")$Data
Lamprey.VT <- GetVT(Lamprey.Data, units = "day")

# problem with step of 0 in row 129 (Weibull only defined for x > 0)
Lamprey.VT$S[Lamprey.VT$S == 0] <- 0.000001

Lamprey.MRW <- list("numT" = nrow(Lamprey.VT), "step" = Lamprey.VT$S, "theta" = Lamprey.VT$Theta)

# single state model
initsSingleState <- function() { list(a = runif(1, 0, 10), 
																			b = runif(1, 0, 10), 
																			mu = runif(1, -3*pi/2, pi/2), 
																			rho = runif(1, 0, 1))}
parametersSingleState <- c("a", "b", "mu", "rho")
system.time(Lamprey.singleState <- jags(Lamprey.MRW, initsSingleState, parametersSingleState, "SingleState.txt", 
																n.chains = 2, n.iter = 50000, n.thin = 25, n.burnin = 10000))
analyzeConvergence(Lamprey.singleState, plotTraces = TRUE)


# double state model
initsDoubleState = function() { list(a = sort(runif(2, 0.5, 2.5)),
																		 b = sort(runif(2, 0, 5)),
																		 mu = runif(2, -3*pi/2, pi/2),
																		 rho = sort(runif(2, 0, 1)) ) }
parametersDoubleState = c("a", "b", "mu", "rho", "idx")
system.time(Lamprey.doubleState <- jags(Lamprey.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
																n.chains = 2, n.iter = 750000, n.thin = 1000, n.burnin = 50000))
analyzeConvergence(Lamprey.doubleState, plotTraces = TRUE)


# double state switch model
initsDoubleStateSwitch <- function() {list (a = sort(runif(2, 0.5, 2.5)), 
																						b = sort(runif(2, 0, 5)), 
																						mu = runif(2, -3*pi/2, pi/2), 
																						rho = sort(runif(2, 0, 1)),
																						p = sort(runif(2, 0, 1), decreasing = TRUE))}
parametersDoubleStateSwitch <- c("a", "b", "mu", "rho", "p", "idx")
system.time(Lamprey.doubleStateSwitch <- jags(Lamprey.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
																			n.chains = 2, n.iter = 100000, n.thin = 50, n.burnin = 10000))
analyzeConvergence(Lamprey.doubleStateSwitch, plotTraces = TRUE)


# triple state switch model
initsTripleStateSwitch <- function() {list (a = sort(runif(3, 0.5, 2.5)), 
																						b = sort(runif(3, 0, 5)), 
																						mu = runif(3, -3*pi/2, pi/2), 
																						rho = sort(runif(3, 0, 1)),
																						p = sort(runif(3, 0, 1), decreasing = TRUE),
																						phi = sort(runif(3, 0, 1), decreasing = TRUE))}
parametersTripleStateSwitch <- c("a", "b", "mu", "rho", "p", "phi", "idx")
system.time(Lamprey.tripleStateSwitch <- jags(Lamprey.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitch.txt",
																			n.chains = 2, n.iter = 200000, n.thin = 100, n.burnin = 40000))
analyzeConvergence(Lamprey.tripleStateSwitch, plotTraces = TRUE)

# NOTE: which chains need to be flipped depend on the results, so this will need to be updated as models are refit!
# Preliminary analysis (plotting traces, etc.) shows states 1 and 3 are flipped between chains for the triple state switch model. 
# Note that another approach is to enforce ordering on some parameter to differentiate states. Some caution must be used with this
# approach to pick a parameter that differs enough across states. When forcing ordering on the Weibull shape parameter, we found that 
# the shape parameters would sometimes be fit similarly but the other parameters would be flipped across chains leading to convergence 
# issues. We demonstrate ordering the Weibull scale parameter in the tripleStateSwitch model for the WFR and Wolf data.
Lamprey.tripleStateSwitch = flipChainsTripleModel(Lamprey.tripleStateSwitch, 1, 1, 3)

save(Lamprey.singleState, file="../../mrw/mrw/data/Lamprey.singleState.rda", compress = TRUE)
save(Lamprey.doubleState, file="../../mrw/mrw/data/Lamprey.doubleState.rda", compress = TRUE)
save(Lamprey.doubleStateSwitch, file="../../mrw/mrw/data/Lamprey.doubleStateSwitch.rda", compress = TRUE)
save(Lamprey.tripleStateSwitch, file="../../mrw/mrw/data/Lamprey.tripleStateSwitch.rda", compress = TRUE)
save(Lamprey.Data, file="../../mrw/mrw/data/Lamprey.Data.rda", compress = TRUE)
save(Lamprey.VT, file="../../mrw/mrw/data/Lamprey.VT.rda", compress = TRUE)
