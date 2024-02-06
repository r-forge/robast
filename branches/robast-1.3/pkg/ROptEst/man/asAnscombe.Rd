\name{asAnscombe}
\alias{asAnscombe}

\title{Generating function for asAnscombe-class}
\description{
  Generates an object of class \code{"asAnscombe"}.
}
\usage{asAnscombe(eff = .95, biastype = symmetricBias(), normtype = NormType())}
\arguments{
  \item{eff}{ value in (0,1]: ARE in the ideal model }
  \item{biastype}{ a bias type of class \code{BiasType}}
  \item{normtype}{ a norm type of class \code{NormType}}
}
\value{Object of class \code{asAnscombe}}
\references{
  F.J. Anscombe (1960). Rejection of Outliers. Technometrics \emph{2}(2): 123-146.
  \doi{10.1080/00401706.1960.10489888}.
  
  F. Hampel et al. (1986). \emph{Robust Statistics}. 
  The Approach Based on Influence Functions. New York: Wiley.
  \doi{10.1002/9781118186435}.

  M. Kohl (2005). \emph{Numerical Contributions to the Asymptotic Theory of Robustness.}
  Dissertation. University of Bayreuth. \url{https://epub.uni-bayreuth.de/id/eprint/839/2/DissMKohl.pdf}.

  H. Rieder (1994). \emph{Robust Asymptotic Statistics.} Springer. 
  \doi{10.1007/978-1-4684-0624-5}.
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@fraunhofer.itwm.de}}
\seealso{\code{\link{asAnscombe-class}}}
\examples{
asAnscombe()

## The function is currently defined as
function(eff = .95, biastype = symmetricBias(), normtype = NormType()){ 
    new("asAnscombe", eff = eff, biastype = biastype, normtype = normtype) }
}
\concept{Hampel risk}
\concept{risk}
\keyword{robust}
