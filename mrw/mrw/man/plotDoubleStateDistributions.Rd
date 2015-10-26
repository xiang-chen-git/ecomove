\name{plotDoubleStateDistributions}
\alias{plotDoubleStateDistributions}
\title{Plot fitted model distributions}
\usage{
  plotDoubleStateDistributions(doubleState, step, theta)
}
\arguments{
  \item{doubleState}{An \code{\link[=rjags-class]{rjags}}
  result for a MRW model with 2 states}

  \item{step}{The vector of steps used to fit the model}

  \item{theta}{The vector of turning angles used to fit the
  model}
}
\description{
  Plots the empirical and fitted distributions of steps and
  turning angles assigned to each state for a MRW model.
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{plotTripleStateDistributions}}
}

