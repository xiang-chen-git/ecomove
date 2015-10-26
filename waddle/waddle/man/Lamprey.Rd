\name{Lamprey}
\alias{Lamprey}
\docType{data}
\title{ Lamprey Data}
\description{
Acoustically tagged lamprey movement data - Hammond Bay, Lake Huron.  Owners M. Wagner and T. Meckley - GLFC)
}
\usage{data(Lamprey)}
\format{
  An object of class \code{track} (inherits from \code{data.frame}) with 434 observations on the following 4 variables.
  \describe{
    \item{\code{Time}}{a POSIXlt}
    \item{\code{X}}{Michigan zone UTM Easting - meters}
    \item{\code{Y}}{Michigan zone UTM Northing - meters}
    \item{\code{Z}}{Depth (distance from surface) - meters}
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
data(Lamprey)
plot(Lamprey)
}
\keyword{datasets}
