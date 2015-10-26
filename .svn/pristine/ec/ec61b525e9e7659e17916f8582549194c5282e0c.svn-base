\name{SmoothTrack}
\alias{SmoothTrack}
\title{Smooth Track}
\usage{
SmoothTrack(Track, steps = 1)
}
\arguments{
  \item{Track}{a movement track (designed for the
  \code{track} class, but will work for any numeric
  matrix)}

  \item{steps}{The number of steps to roll the average
  over.  \code{steps}=1 returns the original data,
  \code{steps}}
}
\description{
Smooth Track
}
\details{
Smooths track with a moving average

This function is robust to a mixture of real and complex
numbers, and will handle date-time columns as long as they
are called \code{Time}.
}
\examples{
data(Lamprey)
par(mfrow=c(2,2))
plot(Lamprey, main="No Smooth")
plot(SmoothTrack(Lamprey, 3), main="3 step smooth")
plot(SmoothTrack(Lamprey, 5), main="5 step smooth")
plot(SmoothTrack(Lamprey, 10), main="10 step smooth")
}
\author{
Eli Gurarie
}

