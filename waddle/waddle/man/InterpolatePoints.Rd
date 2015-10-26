\name{InterpolatePoints}
\alias{InterpolatePoints}
\title{Subsample or interpolate movement points}
\usage{
InterpolatePoints(Data, n = 60, units = c("sec", "min", "hour", "day")[1],
  id = "id")
}
\arguments{
  \item{Data}{a data frame containing (at least) an "X", a
  "Y" and a "Time"}

  \item{n}{time interval of interpolation / subsampling}

  \item{units}{one of \code{"sec", "min", "hour", "day"}}
}
\value{
list with two elements: \item{Data}{The data frame,
regularized - as a \code{track} class object.}
\item{N.interpolated}{The number of points added to the
data frame (via interpolation) or removed from the data
frame (filtering)}
}
\description{
This is just a simplified wrapper for adehabitatLT's
\code{\link[adehabitatLT]{redisltraj}}.
}
\examples{
data(Lamprey)
L0.5 <- InterpolatePoints(Lamprey, 30); L0.5$N.interpolated
L1 <- InterpolatePoints(Lamprey, 60); L1$N.interpolated
L3 <- InterpolatePoints(Lamprey, 180); L3$N.interpolated

plot(L1$Data$Z[1:9], type="o", cex=1.5, asp=1);
points(L3$Data$Z, col=2, type="o", pch=19)
points(L0.5$Data$Z, col=3, pch=19, cex=0.5, type="o")
legend("topright", col=1:3, pch=c(1,19,19), legend=c("1 minute", "3 minutes", "0.5 minutes"), lwd=1)
}
\author{
Eli Gurarie
}

