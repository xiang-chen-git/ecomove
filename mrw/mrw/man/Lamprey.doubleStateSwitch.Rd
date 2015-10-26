\name{Lamprey.doubleStateSwitch}
\alias{Lamprey.doubleStateSwitch}
\docType{data}
\title{Lamprey: JAGS fit of a double-state switch MRW Model}
\description{
The original \code{\link[waddle]{Lamprey}} data was interpolated to a 2-min interval as \code{\link{Lamprey.Data}}. The steps and turning angles were computed as \code{\link{Lamprey.VT}} and used to fit the double-state switch MRW model using JAGS and R.
}
\usage{data(Lamprey.doubleStateSwitch)}
\format{
The data is an \code{\link[=rjags-class]{rjags}} object
}
\source{
\code{\link[waddle]{Lamprey}}
}
\references{
Juan Manuel Morales, Daniel T. Haydon, Jacqui Frair, Kent E. Holsinger, and John M. Fryxell 2004. Extracting more out of relocation data: building movement models as mixtures of random walks. Ecology 85:2436-2445.
}
\examples{
data(Lamprey.doubleStateSwitch)
}
\keyword{datasets}
