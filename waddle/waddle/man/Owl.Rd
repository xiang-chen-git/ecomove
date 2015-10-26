\name{Owl}
\alias{Owl} 
\docType{data}
\title{Eagle owl movement data 
}
\description{
Tracks of eagle owls (Bubo bubo) radiotagged in south-western Spain during the years 2003-2007. (Data from Maria Delgado).
}
\usage{data(Owl)}
\format{
  A data frame with 337 observations on the following 10 variables.
  \describe{
    \item{\code{ID}}{ID of owls - the six longest tracks were selected: \code{Owl305} \code{Owl345} \code{Owl365} \code{Owl385} \code{Owl464} \code{Owl5341}}
    \item{\code{Sex}}{a factor with levels \code{F} and \code{M}}
    \item{\code{Phase}}{a factor with levels \code{Wandering} } (when individuals explore different                      areas before settling in a new area) and \code{Settling (individual settled in a stable area with established home range)}
    \item{\code{Date}}{a POSIXct}
    \item{\code{X}, \code{Y}}{coordinates of owl location (m)}
    \item{\code{X.nest}, \code{Y.nest}}{coordinates of nest location}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{Maria Delgado}
\references{}
\examples{
data(Owl)
Owl$N <- rep(table(Owl$ID), table(Owl$ID))
Owl$LABEL <-  factor(paste(Owl$ID, " (n = ", Owl$N, ")", sep=""))
require(ggplot2)
qplot(X/1000, Y/1000, geom="path", facets = ~ LABEL, data=Owl, asp=1, col=Phase ,xlab="X (km)", ylab="Y (km)")
}
\keyword{datasets}
