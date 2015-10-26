\name{DiagPlot.segments}
\alias{DiagPlot.segments}
\title{Diagnostic Plot of BPMM output}
\usage{
DiagPlot.segments(Partition, plot = TRUE, echo = FALSE, ...)
}
\arguments{
  \item{Data.traj}{Trajectory class to analyze.}

  \item{plot}{whether to produce the diagnostic plots.}

  \item{echo}{whether to return the resisuals.}
}
\value{
Returns the residuals of the model. Produces a qqnorm and
histogram of the standardized model fit, with a standard
normal distribution curve overlaid.
}
\description{
Residual diagnostic plots are a quick way to assess whether
the residuals have a standard normal distribution and are
independent are an appropriate model for the BPMM fit.
Also returns the residuals.
}

