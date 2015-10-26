\name{Z.like1D}
\alias{Z.like1D}
\alias{Z.like2D,}
\alias{Z.likeBlock}
\title{Likelihood functions for cvm estimation}
\usage{
Z.like1D(p, Z, T)
}
\arguments{
  \item{p}{numeric vector of either form c(nu, tau, v0) or
  c(nu, tau) if v0 is provided}

  \item{Z,T}{one-dimensional location and time vectors}
}
\description{
Likelihood for estimating cvm parameters for one
dimensional Z vector.
}
\examples{
# Simulate some 1-D data

nu <- 10
tau <- 3
v0 <- 20
mycvm <- CVM2(nu, tau, T = cumsum(rexp(50)), v0)
X <- Re(myvm$Z)
T <- mycvm$T

# Obtain and plot marginal likelihoods with correct values

LikelihoodScan(X,T,nu,tau,v0,Z.like1)
LikelihoodScan(X,T,nu,tau,v0,Z.like2, k=5)

# Obtain MLE

nu <- 3; tau <- 0.5; v0 <- 0
T <- cumsum(rexp(50))
Z <- Re(CVM2(T, nu, tau, v0)$Z)
plot(T,Z, type="o")

optim(c(1,1), Z.likelihood, Z=Z, T=T, control=list(fnscale=-1))
}

