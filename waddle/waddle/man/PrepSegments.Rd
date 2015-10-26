\name{PrepSegments}
\alias{PrepSegments}
\title{Preliminary segmentation analysis}
\usage{
  PrepSegments(Data.traj, sd = 1, Km = 30, plotit = TRUE,
    nmodels = 10, log = FALSE, mumin = 0, ...)
}
\arguments{
  \item{Data.traj}{trajectory}

  \item{sd}{standard deviation of step response}

  \item{Km}{the maximum number of partitions of the
  trajectory}

  \item{plotit}{whether to plot the likelihood analysis}

  \item{nmodels}{number of candidate models}

  \item{log}{Whether to perform the analysi on the log of
  the step lengths.}
}
\value{
  a list with the following elements: \item{Data.traj}{a
  regularized trajectory} \item{nK}{the optimal number of
  partitions} \item{mus}{the mean values of all the
  candidate models} \item{models}{the index of the selected
  models}
}
\description{
  Takes a trajectory and determines the number of
  partitions of a trajectory based on Markov models. The
  response variable here is limited to the step lengths.
  This is a wrapper for the partitioning tools in
  \code{\link{adehabitatLT}} by Calenge.
}
\seealso{
  \code{\link{modpartltraj}}
}

