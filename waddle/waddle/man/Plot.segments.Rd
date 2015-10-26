\name{plot.segments}
\alias{plot.segments}
\title{Plot Segments}
\usage{
plot.segments(Partition, mean.col = "red", cp.col = "orange", cp.lwd = 1,
  ...)
}
\arguments{
  \item{Partition}{This is a list containing the
  partitioned model (pm), and the output of the
  PrepSegments function.}

  \item{{mean.col}}{color of horizonal mean segments}

  \item{{cp.col,cp.lwd}}{color and width of the vertical
  change point demarcations}

  \item{...}{additional parameters to be passed to the plot
  function.}
}
\value{
No value is returned.  A plot is produced with the time
series and vertical changepoint segments.
}
\description{
This function plots the time series and change points for
the BPMM output.
}

