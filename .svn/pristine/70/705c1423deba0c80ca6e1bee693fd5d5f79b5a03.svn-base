\name{flipChainsTripleModel}
\alias{flipChainsTripleModel}
\title{Flip parameter indices in MCMC chains}
\usage{
  flipChainsTripleModel(jagsResults, chainIndex, idxA,
    idxB)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}

  \item{chainIndex}{Which chain within the
  \code{jagsResults} to flip}

  \item{idxA}{Which parameter index (1, 2, or 3) to flip}

  \item{idxB}{Which parameter index (1, 2, or 3) to flip}
}
\value{
  An rjags object with the specified a, b, mu, rho, p, and
  phi parameter indicies flipped for the specified chain.
  The parameters, as well as the idx state are flipped in
  the \code{sims.array} array, but not in the
  \code{sims.matrix} or \code{sims.list}. Also, the
  calculated paramter means etc. in the \code{BUGSoutput}
  ##' are not updated.
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
  \link{flipChainsDoubleModel}
}

