\name{WFR.doubleStateSwitch}
\alias{WFR.doubleStateSwitch}
\docType{data}
\title{Wild Forest Reindeer: JAGS fit of a double-state switch MRW Model}
\description{
The original \code{\link[waddle]{WFR}} data was interpolated to a 2-day interval with the interpolated points jittered to avoid linear
segments as \code{\link{WFR.Data}}. The steps and turning angles were computed as \code{\link{WFR.VT}} and used to fit the 
double-state switch MRW model using JAGS and R.
}
\usage{data(WFR.doubleState)}
\format{
The data is an \code{\link[=rjags-class]{rjags}} object
}
\source{
\code{\link[waddle]{WFR}}
}
\references{
Juan Manuel Morales, Daniel T. Haydon, Jacqui Frair, Kent E. Holsinger, and John M. Fryxell 2004. Extracting more out of relocation data: building movement models as mixtures of random walks. Ecology 85:2436-2445.
}
\examples{
data(WFR.doubleStateSwitch)
}
\keyword{datasets}
