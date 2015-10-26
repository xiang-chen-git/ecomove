\name{BCRW}
\alias{BCRW}
\alias{BCRW,}
\alias{multiBCRW}
\title{Biased Correlated Random Walk functions}
\usage{
BCRW(n = 50, a = 2, b = 1, rho = 0.95, Z.center = 0, attraction = 0.5, Z0 = 0)
 multiBCRW(rhos, attractions, Z.centers, ns, ...)
}
\arguments{
  \item{n}{number of steps}

  \item{{a,b}}{shape and scale parameters of the Weibull
  distribution}

  \item{rho}{clustering parameter for the turning angles}

  \item{Z.center}{center of attraction (complex)}

  \item{attraction}{strength of attraction}

  \item{Z0}{initial location.}

  \item{{ns,rhos,attractions,Z.centers}}{vector of lengths,
  clustering coeffiecients, attraction coefficients, and
  center locations (multiBCRW)}

  \item{...}{additional arguments for BCRW function
  (multiBCRW)}
}
\value{
BCRW returns a trajectory - as a (`track' class) data frame
containg Z (complex vector), X, and Y locations.

`multiBCRW' returns a `multipath' class object containing a
vector Z of locations and a Phase vector enumerating the
phases of the locations. The multipath object can be
plotted with the `plot.multipath' method
}
\description{
Generates a discrete 2D correlated random walk that is
biased towards a given center point.
}
\details{
The step lengths of this BCRW have a Weibull(shape = a,
scale = b) distribution, the turning angles have a wrapped
Cauchy (mu_i, rho) distribution, where mu_i is given by: (1
- A) phi_i + A theta_{i}, where phi_i is the orientation of
the previous step and theta_i represents the angle pointed
towards the center of attraction.  When the attraction
parameter A = 1, the movement is always towards the center
of attraction.  If A = 0, the movement is a standard
unbiased CRW.
}
\examples{
# BCRW examples
par(mfrow=c(2,2), bty="l", mar=c(4,4,1,1))
plot(BCRW(n = 500, rho=0.9, attraction = 0.9))
plot(BCRW(n = 500, rho=0.9, attraction = 0.2))
plot(BCRW(n = 500, rho=0.2, attraction = 0.9))
plot(BCRW(n = 500, rho=0.2, attraction = 0.2))


# multiBCRW examples
multiZ <- multiBCRW(c(0.9, 0.2, 0.2, 0.9), c(0.9, 0.2, 0.9, 0.2), c(0, 10-10i, 20+10i, 0), ns = c(100,100,100,100))
par(mfrow=c(1,1))
plot(multiZ)
}

