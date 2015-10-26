\name{calculateConvergence}
\alias{calculateConvergence}
\title{Calculate convergence statistics}
\usage{
  calculateConvergence(chain, multipleChains = FALSE)
}
\arguments{
  \item{chain}{A \code{\link{mcmc}} result}

  \item{multipleChains}{\code{TRUE} if chain contains
  multiple chains, \code{FALSE} if it is a single chain}
}
\value{
  A data frame with parameter and convergence statistics
}
\description{
  Returns parameter means and standard deviations, as well
  as several convergence statistics: effective parameter
  size, autocorrelation at lag 1, Gelman and Rubin's
  diagnostic, and Geweke's Z.
}
\author{
  Chloe Bracis
}

