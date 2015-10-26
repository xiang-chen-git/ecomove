\name{multiCVM}
\alias{multiCVM}
\title{Multi-phase CVM model}
\usage{
multiCVM(taus, nus, Ts)
}
\arguments{
  \item{{taus,nus,Ts}}{vector values of parameter values
  for the multiple phases.  Ts refers to the duration of
  each phase.  Note that the sampling is assumed to occur
  at integer time intervals}
}
\description{
Multi-phase CVM
}
\details{
Uses the correlated velocity movement continuous time model
(CVM) parameterized in terms of the mean speed `nu' and
characteristic time scale `tau' to generate a multi-phase
model, useful for testing robustness of various change
point methods
}
\examples{
# Time scale changes

taus <- c(2,20,40,2)
nus <- rep(1,4)
Ts <- c(100,50,50,100)
Tau.sim <- multiCVM(taus, nus, Ts)
plot(Tau.sim)

# Mean speed changes

nus <- c(1,5,10,1)
taus <- rep(2,4)
Ts <- c(100,50,50,100)
Nu.sim <- multiCVM(taus, nus, Ts)
plot(Nu.sim)
}

