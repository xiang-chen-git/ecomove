rm(list=ls())
require(waddle)
require(rjags)
require(R2jags)

# subsample reindeer to daily time step

data(WFR)

# gaps
gaps <- which(diff(WFR$Time) / 60 / 24 > 4) # gaps over 4 days

# Interpolate to 2 day time step
WFR.Data <- InterpolatePoints(WFR, nsec = 60*60*48, id = "WFR")$Data

interpPts <- NULL
jitCoords <- NULL
for (i in gaps)
{
  pts <- which(WFR.Data$Time > WFR$Time[i] & WFR.Data$Time < WFR$Time[i+1])
  interpPts <- c(interpPts, pts )
  # scale of jitter adapted to each interpolated section (vs. doing all at once)
  # calculate amount based on dist, not X and Y diff, which will depend on orientation
  amount <- min(Mod(diff(WFR.Data$Z[pts]))) / 5
  jitCoords <- rbind(jitCoords, data.frame(X = jitter(WFR.Data$X[pts], amount = amount), Y = jitter(WFR.Data$Y[pts], amount = amount)) )
}

WFR.Data[interpPts, c("X", "Y", "Z")] <- cbind(jitCoords, Z = complex(re = jitCoords$X, im = jitCoords$Y))

WFR.VT <- GetVT(WFR.Data, units = "day")

WFR.MRW <- list("numT" = nrow(WFR.VT), "step" = WFR.VT$S, "theta" = WFR.VT$Theta)

# single state model
# Here we are using an alternate model file that gives different boundaries for the circular distribution
# Because JAGS does not include a circular distribution, there are apparant convergence issues if the estimated
# parameter value is close to the boundary, which is resolved by shifting the boundary.
initsSingleState <- function() { list(a = runif(1, 0, 10), 
																			b = runif(1, 0, 10), 
																			mu = runif(1, -pi/2, 3*pi/2), 
																			rho = runif(1, 0, 1))}
parametersSingleState <- c("a", "b", "mu", "rho")
system.time(WFR.singleState <- jags(WFR.MRW, initsSingleState, parametersSingleState, "SingleStateAlt.txt", 
																n.chains = 2, n.iter = 50000, n.thin = 25, n.burnin = 10000))
analyzeConvergence(WFR.singleState, plotTraces = TRUE)


# double state model
initsDoubleState = function() { list(a = sort(runif(2, 0.5, 2.5)),
																		 b = sort(runif(2, 0, 5)),
																		 mu = runif(2, -3*pi/2, pi/2),
																		 rho = sort(runif(2, 0, 1)) ) }
parametersDoubleState <- c("a", "b", "mu", "rho", "idx")
system.time(WFR.doubleState <- jags(WFR.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
																n.chains = 2, n.iter = 100000, n.thin = 50, n.burnin = 10000))
analyzeConvergence(WFR.doubleState, plotTraces = TRUE)


# double state switch model
initsDoubleStateSwitch <- function() {list (a = sort(runif(2, 0.5, 2.5)), 
																						b = sort(runif(2, 0, 5)), 
																						mu = runif(2, -3*pi/2, pi/2), 
																						rho = sort(runif(2, 0, 1)),
																						p = sort(runif(2, 0, 1), decreasing = TRUE))}
parametersDoubleStateSwitch <- c("a", "b", "mu", "rho", "p", "idx")
system.time(WFR.doubleStateSwitch <- jags(WFR.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
																			n.chains = 2, n.iter = 400000, n.thin = 200, n.burnin = 20000))
analyzeConvergence(WFR.doubleStateSwitch, plotTraces = TRUE)


# triple state switch model
# problems with convergence since states switch throughout chain, so try ordering b's
initsTripleStateSwitch <- function() {list (a = sort(runif(3, 0.5, 2.5)), 
																						b0 = sort(runif(3, 0, 5)), 
																						mu = runif(3, -3*pi/2, pi/2), 
																						rho = sort(runif(3, 0, 1)),
																						p = sort(runif(3, 0, 1), decreasing = TRUE),
																						phi = sort(runif(3, 0, 1), decreasing = TRUE))}
parametersTripleStateSwitch <- c("a", "b", "mu", "rho", "p", "phi", "idx")
system.time(WFR.tripleStateSwitch <- jags(WFR.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitchOrderedB.txt",
																			n.chains = 2, n.iter = 1000000, n.thin = 1000, n.burnin = 250000))
analyzeConvergence(WFR.tripleStateSwitch, plotTraces = TRUE)


save(WFR.singleState, file="../../mrw/mrw/data/WFR.singleState.rda", compress = TRUE)
save(WFR.doubleState, file="../../mrw/mrw/data/WFR.doubleState.rda", compress = TRUE)
save(WFR.doubleStateSwitch, file="../../mrw/mrw/data/WFR.doubleStateSwitch.rda", compress = TRUE)
save(WFR.tripleStateSwitch, file="../../mrw/mrw/data/WFR.tripleStateSwitch.rda", compress = TRUE)
save(WFR.Data, file="../../mrw/mrw/data/WFR.Data.rda", compress = TRUE)
save(WFR.VT, file="../../mrw/mrw/data/WFR.VT.rda", compress = TRUE)

