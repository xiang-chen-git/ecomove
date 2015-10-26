\name{Lamprey.VT}
\alias{Lamprey.VT}
\docType{data}
\title{Lamprey: Interpolated Data}
\description{
  The original \code{\link[waddle]{Lamprey}} data was interpolated to a 2-min interval as \code{Lamprey.Data}. This data set contains the calculated trajectory steps and turning angles.
}
\usage{data(Lamprey.VT)}
\format{
  A data frame with 338 observations on the following 11 variables.
  \describe{
    \item{\code{Z.start}}{location of step start (complex)}
    \item{\code{Z.end}}{location of step end (complex)}
    \item{\code{S}}{step length}
    \item{\code{Phi}}{absolute angle}
    \item{\code{Theta}}{turning angle}
    \item{\code{T.start}}{time of step start}
    \item{\code{T.end}}{time of step end}
    \item{\code{T.mid}}{time of midpoint of step}
    \item{\code{dT}}{step duration}
    \item{\code{V}}{speed}
    \item{\code{T.POSIX}}{Datetime of step}
  }
}
\source{
  \code{\link[waddle]{Lamprey}}
}
\examples{
  data(Lamprey.VT)
}
\keyword{datasets}
