\name{flipChainsDoubleModel}
\alias{flipChainsDoubleModel}
\title{Flip parameter indices in MCMC chains}
\usage{
  flipChainsDoubleModel(jagsResults, chainIndex)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}

  \item{chainIndex}{Which chain within the
  \code{jagsResults} to flip}
}
\value{
  An \code{\link[=rjags-class]{rjags}} object with the a,
  b, mu, rho, and (optionally) p parameter indicies 1 and 2
  flipped for the specified chain. The parameters, as well
  as the idx state are flipped in the \code{sims.array}
  array, but not in the \code{sims.matrix} or
  \code{sims.list}. Also, the calculated paramter means
  etc. in the \code{BUGSoutput} are not updated.
}
\description{
  This method takes an \code{\link[=rjags-class]{rjags}}
  object and flips the parameter indicies, then returns it.
  This is useful if the states are reversed from one chain
  to the next, i.e. state 1 is the more active state in one
  chanin but the more sedentatry state in another. However,
  the paramters and idx state variables are only flipped in
  the \code{sims.array} part of the
  \code{\link[=rjags-class]{rjags}} object, so be careful
  not to use the calculated paramter statistics or other
  chain representations.
}
\author{
  Chloe Bracis
}
\seealso{
  \link{flipChainsTripleModel}
}

