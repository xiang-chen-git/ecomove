\name{getParameterValues}
\alias{getParameterValues}
\title{Parameter summary}
\usage{
  getParameterValues(jagsResults)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}
}
\value{
  A vector of model parameter values
}
\description{
  Returns the parameter mean and standard error for the
  Weibull and wrapped Cauchy distributions in the mixed
  random walk model. Parameters not present in a model are
  returned as NA.
}
\author{
  Chloe Bracis
}

