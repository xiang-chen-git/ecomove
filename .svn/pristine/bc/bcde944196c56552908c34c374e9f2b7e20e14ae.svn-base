\name{Multipaths: Tau.sim, Nu.sim, BCRW.sim}
\alias{Multipaths}
\alias{Tau.sim}
\alias{Nu.sim}
\alias{BCRW.sim}
\docType{data}
\title{Simulated multi-phase trajectories}
\description{Three simulated trajectories with known change points.}

\usage{data(Multipaths)}
\format{
  The three tracks are \code{Tau.sim}, \code{Nu.sim}, \code{BCRW.sim} - all \code{\link{multipath}} object with 300 observations on the following 2 variables.
  \describe{
    \item{\code{Z}}{complex vector of locations.}
    \item{\code{Phases}}{corresponding vector of the phases (integers 1-4).}
  }
}
\details{
All three paths consist of 300 observations, with change points occuring at times 100, 150 and 200, i.e. with four phases.  In all three, Phases I and IV have the same parameters and reflect more intensive (slower / tortuous / spatially constrained) space use, while II and III represent transitional movements with more subtle differences. 

Nu.sim and Tau.simare based on the continuous time correlated velocity movement (CVM) model, specified in terms of the mean speed (\eqn{\nu}) and the characteristic time scale of autocorrelation (\eqn{\tau}), where smaller values of \eqn{\tau} indicate more tortuous movements, while larger values indicate more linear movements.   In Nu.sim, the velocity changes from low (1), to high (5), to higher (10), to low again (1).   In Tau.sim the time scale changes from low (2), to high (10), to higher (100), and back to low (2). 

BCRW.sim is based on a discrete biased correlated random walk (see \code{\link{BCRW}}). In this simulation, the concentration parameter and the step length parameters are constant throughout, while the attraction strength is greater during the second and third phases, the attraction point itself shifts twice as well.  
}
\source{
Code to generate these tracks can be found in the help files for \code{\link{multiBCRW}} and \code{\link{multiCVM}}.
}
\references{}
\examples{
data(Multipaths)
plot(Nu.sim)
plot(Tau.sim)
plot(BCRW.sim)
}
\keyword{datasets}
