\name{Wolf.doubleStateSwitch}
\alias{Wolf.doubleStateSwitch}
\docType{data}
\title{Wolf: JAGS fit of a double-state switch MRW Model}
\description{
The original \code{\link[waddle]{Wolf}} data was averaged to a 1-day interval as \code{\link{Wolf.Data}}. The steps and turning angles were computed as \code{\link{Wolf.VT}} and used to fit the double-state switch MRW model using JAGS and R.
}
\usage{data(Wolf.doubleState)}
\format{
The data is an \code{\link[=rjags-class]{rjags}} object
}
\source{
\code{\link[waddle]{Wolf}}
}
\references{
Juan Manuel Morales, Daniel T. Haydon, Jacqui Frair, Kent E. Holsinger, and John M. Fryxell 2004. Extracting more out of relocation data: building movement models as mixtures of random walks. Ecology 85:2436-2445.
}
\examples{
data(Wolf.doubleStateSwitch)
}
\keyword{datasets}
