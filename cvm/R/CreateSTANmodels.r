setwd("c:/eli/programming/cvm/R")
require(rstan)
system.time(CVMstan2D <- stan_model(file = "cvmlikelihood2D.stan"))
save(CVMstan2D, file="../cvm/data/CVMstan2D.Rda")
