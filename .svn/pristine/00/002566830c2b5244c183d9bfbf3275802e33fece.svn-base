\name{CompileResults.mrw}
\alias{CompileResults.mrw}
\title{Compile Multi-state random walk results}
\usage{
CompileResults.mrw(animal, flip = NULL, tripleflip = NULL)
}
\arguments{
  \item{animal}{a character string with the animal name
  (must be one of the available results of the mrw
  analyses).}

  \item{flip}{which of the chains to reassign - can be a
  vector including up to 1:3.  The Double and DoubleSwitch
  chain are flipped easily, whereas ...}

  \item{tripleflip}{is a vector with 2 or 3 elements that
  determines which of the three-state chains should be
  flipped.  If the vector has three elements , then the
  first two are flipped, and then the second and the third,
  so: "1,2,3" change an "ABC" first to a "BAC" and then to
  a "CAB".}
}
\value{
a data frame with the following columns: \item{Time}{POSIX
Time} \item{Z1}{initial location} \item{Z2}{next location}
\item{V}{Speed of step} \item{Double}{Mode of states (1 or
2) in the converged chain for the double state model.}
\item{DoubleSwitch}{Mode of states (1 or 2) in the
converged chain for the double state switching model.}
\item{TripleSwitch}{Mode of states (1, 2 or 3) in the
converged chain for the triple state switching model.}
}
\description{
Loads the results of the analysis from the \code{mrw}
package
}
\seealso{
\code{\link{plot.mrw}}
}

