\name{plotStatesCompare}
\alias{plotStatesCompare}
\title{Plot states}
\usage{
  plotStatesCompare(jagsResults, nstates = 2)
}
\arguments{
  \item{jagsResults}{An \code{\link[=rjags-class]{rjags}}
  result}

  \item{nstates}{The number of states in the model (either
  2 or 3)}
}
\description{
  Plots assigned state for each step for a MRW model,
  comparing different methods of determining state for each
  step across iterations (mean, median, mode, iteration
  most similar to mode). Note that for 3 state models, the
  mean and median can be problematic.
}
\author{
  Chloe Bracis
}
\seealso{
  \code{\link{getMeanStates}},
  \code{\link{getMedianStates}},,
  \code{\link{getModeStates}},
  \code{\link{getIterationSimilarModeStates}},
  \code{\link{plotStateProbabilities}}
}

