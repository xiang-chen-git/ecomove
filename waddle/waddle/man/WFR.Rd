\name{WFR}
\alias{WFR}
\docType{data}
\title{
Wild Forest Reindeer 
}
\description{
GPS collared wild forest reindeer (\emph{Ranigfer tarandus fennicus}) movements in eastern Finland. (owner J. Pusenius - RKTL)
}
\usage{data(WFR)}
\format{
  An object of class \code{track} (inherits from \code{data.frame}) with 4974 observations on the following 4 variables.
  \describe{
    \item{\code{Time}}{POSIXlt time (GMT zime zone)}
    \item{\code{X}}{Finland UTM Easting (m/1000 = km)}
    \item{\code{Y}}{Finland UTM Northing (m/1000 = km)}
  }
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(WFR)
plot(WFR$X, WFR$Y, type="l", asp=1)
}
\keyword{datasets}
