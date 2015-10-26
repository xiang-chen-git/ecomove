rm(list=ls())
require(waddle)
require(rjags)
require(R2jags)
setwd("../MultipleBehaviorsAnalysis/mrw")

data(Multipaths)

# Nu
Nu.Data <- data.frame(Time = 1:length(Nu.sim$Z), X = Re(Nu.sim$Z), Y = Im(Nu.sim$Z))
Nu.VT <- GetVT(Nu.Data, units = "day")
Nu.MRW <- list("numT" = nrow(Nu.VT), "step" = Nu.VT$S, "theta" = Nu.VT$Theta)

# Tau
Tau.Data <- data.frame(Time = 1:length(Tau.sim$Z), X = Re(Tau.sim$Z), Y = Im(Tau.sim$Z))
Tau.VT <- GetVT(Tau.Data, units = "day")
Tau.MRW <- list("numT" = nrow(Tau.VT), "step" = Tau.VT$S, "theta" = Tau.VT$Theta)

# BCRW
BCRW.Data <- data.frame(Time = 1:length(BCRW.sim$Z), X = Re(BCRW.sim$Z), Y = Im(BCRW.sim$Z))
BCRW.VT <- GetVT(BCRW.Data, units = "day")
BCRW.MRW <- list("numT" = nrow(BCRW.VT), "step" = BCRW.VT$S, "theta" = BCRW.VT$Theta)

# double state model
initsDoubleState = function() { list(a = sort(runif(2, 0.5, 2.5)),
                                     b = sort(runif(2, 0, 5)),
                                     mu = runif(2, -3*pi/2, pi/2),
                                     rho = sort(runif(2, 0, 1)) ) }
parametersDoubleState = c("a", "b", "mu", "rho", "idx")

system.time(Nu.doubleState <- jags(Nu.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
                                        n.chains = 2, n.iter = 50000, n.thin = 100, n.burnin = 5000))
system.time(Tau.doubleState <- jags(Tau.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
                                        n.chains = 2, n.iter = 150000, n.thin = 300, n.burnin = 5000))
system.time(BCRW.doubleState <- jags(BCRW.MRW, initsDoubleState, parametersDoubleState, "DoubleState.txt",
                                        n.chains = 2, n.iter = 50000, n.thin = 100, n.burnin = 5000))

analyzeConvergence(Nu.doubleState, plotTraces = TRUE)
analyzeConvergence(Tau.doubleState, plotTraces = TRUE)
analyzeConvergence(BCRW.doubleState, plotTraces = TRUE)

# double state switch model
initsDoubleStateSwitch <- function() {list (a = sort(runif(2, 0.5, 2.5)), 
                                            b = sort(runif(2, 0, 5)), 
                                            mu = runif(2, -3*pi/2, pi/2), 
                                            rho = sort(runif(2, 0, 1)),
                                            p = sort(runif(2, 0, 1), decreasing = TRUE))}
parametersDoubleStateSwitch <- c("a", "b", "mu", "rho", "p", "idx")

system.time(Nu.doubleStateSwitch <- jags(Nu.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
                                         n.chains = 2, n.iter = 50000, n.thin = 100, n.burnin = 5000))
system.time(Tau.doubleStateSwitch <- jags(Tau.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
                                          n.chains = 2, n.iter = 150000, n.thin = 300, n.burnin = 5000))
system.time(BCRW.doubleStateSwitch <- jags(BCRW.MRW, initsDoubleStateSwitch, parametersDoubleStateSwitch, "DoubleStateSwitch.txt",
                                           n.chains = 2, n.iter = 50000, n.thin = 100, n.burnin = 5000))

analyzeConvergence(Nu.doubleStateSwitch, plotTraces = TRUE)
analyzeConvergence(Tau.doubleStateSwitch, plotTraces = TRUE)
analyzeConvergence(BCRW.doubleStateSwitch, plotTraces = TRUE)

# triple state switch model
initsTripleStateSwitch <- function() {list (a = sort(runif(3, 0.5, 2.5)), 
                                            b0 = sort(runif(3, 0, 5)), 
                                            mu = runif(3, -3*pi/2, pi/2), 
                                            rho = sort(runif(3, 0, 1)),
                                            p = sort(runif(3, 0, 1), decreasing = TRUE),
                                            phi = sort(runif(3, 0, 1), decreasing = TRUE))}
parametersTripleStateSwitch <- c("a", "b", "mu", "rho", "p", "phi", "idx")

system.time(Nu.tripleStateSwitch <- jags(Nu.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitchOrderedB.txt",
                                              n.chains = 2, n.iter = 500000, n.thin = 2000, n.burnin = 50000))
system.time(Tau.tripleStateSwitch <- jags(Tau.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitchOrderedB.txt",
                                              n.chains = 2, n.iter = 1500000, n.thin = 3000, n.burnin = 500000))
system.time(BCRW.tripleStateSwitch <- jags(BCRW.MRW, initsTripleStateSwitch, parametersTripleStateSwitch, "TripleStateSwitchOrderedB.txt",
                                              n.chains = 2, n.iter = 100000, n.thin = 1000, n.burnin = 10000))

analyzeConvergence(Nu.tripleStateSwitch, plotTraces = TRUE)
analyzeConvergence(Tau.tripleStateSwitch, plotTraces = TRUE)
analyzeConvergence(BCRW.tripleStateSwitch, plotTraces = TRUE)

# flipping, only need to do before save

Nu.doubleState = flipChainsDoubleModel(Nu.doubleState, 1)
Tau.doubleState = flipChainsDoubleModel(Tau.doubleState, 1)

Nu.doubleStateSwitch = flipChainsDoubleModel(Nu.doubleStateSwitch, 1)
Tau.doubleStateSwitch = flipChainsDoubleModel(Tau.doubleStateSwitch, 1)


# saving data objects

save(Nu.doubleState, file="../../mrw/mrw/data/Nu.doubleState.rda", compress = TRUE)
save(Tau.doubleState, file="../../mrw/mrw/data/Tau.doubleState.rda", compress = TRUE)
save(BCRW.doubleState, file="../../mrw/mrw/data/BCRW.doubleState.rda", compress = TRUE)


#BCRW.doubleStateSwitch = flipChainsDoubleModel(BCRW.doubleStateSwitch, 1)
save(Nu.doubleStateSwitch, file="../../mrw/mrw/data/Nu.doubleStateSwitch.rda", compress = TRUE)
save(Tau.doubleStateSwitch, file="../../mrw/mrw/data/Tau.doubleStateSwitch.rda", compress = TRUE)
#save(BCRW.doubleStateSwitch, file="../../mrw/mrw/data/BCRW.doubleStateSwitch.rda", compress = TRUE)
# Tau might need a little linger, correlation in a2

save(Nu.tripleStateSwitch, file="../../mrw/mrw/data/Nu.tripleStateSwitch.rda", compress = TRUE)
save(Tau.tripleStateSwitch, file="../../mrw/mrw/data/Tau.tripleStateSwitch.rda", compress = TRUE)
save(BCRW.tripleStateSwitch, file="../../mrw/mrw/data/BCRW.tripleStateSwitch.rda", compress = TRUE)

