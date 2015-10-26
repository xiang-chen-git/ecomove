\name{Bowhead}
\alias{Bowhead}
\docType{data}
\title{Bowhead GPS data}
\description{GPS data on bowhead movements from Disko Bay, west Greenland.}
\usage{data(Bowhead)}
\format{
  A data frame with 954 observations on the following 7 variables.
  \describe{
    \item{\code{DateTime}}{a POSIXct}
    \item{\code{Long}}{longitude}
    \item{\code{Lat}}{latitude}
    \item{\code{Hours}}{hours from first observations}
    \item{\code{x.km, y.km}}{x and y coordinates in km, with the mean location as origin}
    \item{\code{z.km}}{location in km units as complex number.}
  }
}
\details{
GPS tagged female bowhead whale tracked in western Greenland (Disko Bay) (see Heide-Jørgensen et al. 2013 for details). The tag was a Fastloc GPS retrievable data and dive logger (www.wildlifecomputers.com), the track consisted of 954 locations collected between April 28 and May 28 2008.
}
\source{}

\references{Heide-Jørgensen, M., K. Laidre, N. Nielsen, R. Hansen, and A. Røstad. 2013. Winter and spring 563 diving behavior of bowhead whales relative to prey. Animal Biotelemetry 1:1–14}
\examples{
data(Bowhead)

# googlemap (requires internet access)
require(ggmap)
DiskoBay <- get_map(location = c(-53.4, 69), maptype = "terrain", zoom=8)
ggmap(DiskoBay) + geom_path(data = Bowhead, mapping = aes(x = Long, y = Lat))
}
\keyword{datasets}
