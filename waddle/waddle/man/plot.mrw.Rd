\name{plot.mrw}
\alias{plot.mrw}
\title{Plot results of multi-state random walk analysis}
\usage{
plot.mrw(Results, cols = c("white", "grey20", "grey50"), plotv = TRUE,
  v.col = "red", y.labels = c("A", "B", "C"), v.lab = TRUE,
  x.axis = TRUE, xlim = range(Results$Time), ...)
}
\arguments{
  \item{Results}{a results data.frame, outputted from
  \link{CompileResults.mrw}}

  \item{cols}{colors of states 1, 2 and 3 (default dark
  grey, light grey and white)}

  \item{plotv}{whether or not to plot the velocity on top
  of the figure.}

  \item{v.col}{color of the response variable (velocity)}

  \item{y.labels}{labels of the three models. Most
  descriptive would be something like: \code{c("Two-State",
  "Two-State \n Switching Model", "Three-State \n Switching
  Model")}}

  \item{v.lab}{whether or not to label the right y-axis.}

  \item{x.axis}{whether or not to label the x axis.}

  \item{...}{further parameters to pass to the plotting of
  the velocity function}
}
\description{
Plots the results of the analysis compiled by the
\code{CompileResults.mrw} function, derived from those
results saved in the \code{mrw} package
}
\seealso{
\code{\link{CompileResults.mrw}}
}

