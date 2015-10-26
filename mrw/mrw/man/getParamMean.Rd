\name{getParamMean}
\alias{getParamMean}
\title{Parameter mean}
\usage{
  getParamMean(jagsResults, paramName)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}

  \item{paramName}{The exact name of the parameter
  (matching those used in \code{BUGSoutput})}
}
\value{
  The mean value of the parameter across chains and
  iterations
}
\description{
  Returns the mean value of the parameter across chains and
  iterations. This is useful if the parameter indicies have
  been flipped in a chain so that the calculated means in
  the \code{BUGSoutput} are not valid.
}
\author{
  Chloe Bracis
}

