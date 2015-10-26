\name{getParamSD}
\alias{getParamSD}
\title{Parameter standard deviation}
\usage{
  getParamSD(jagsResults, paramName)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}

  \item{paramName}{The exact name of the parameter
  (matching those used in \code{BUGSoutput})}
}
\value{
  The standard deviation of the parameter across chains and
  iterations
}
\description{
  Returns the standard deviation value of the parameter
  across chains and iterations. This is useful if the
  parameter indicies have been flipped in a chain so that
  the calculated values in the \code{BUGSoutput} are not
  valid.
}
\author{
  Chloe Bracis
}

