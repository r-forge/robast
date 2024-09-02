\name{asL1}
\alias{asL1}

\title{Generating function for asMSE-class}
\description{
  Generates an object of class \code{"asMSE"}.
}
\usage{asL1(biastype = symmetricBias(), normtype = NormType())}
\arguments{
  \item{biastype}{a bias type of class \code{BiasType}}
  \item{normtype}{ a norm type of class \code{NormType}}
}

\value{Object of class \code{"asMSE"}}
\references{ 
  P. Ruckdeschel and H. Rieder (2004). Optimal Influence Curves for
  General Loss Functions. Statistics & Decisions \emph{22}, 201-223.
  \doi{10.1524/stnd.22.3.201.57067}
}
\author{Peter Ruckdeschel \email{peter.ruckdeschel@uni-oldenburg.de}}
\seealso{\code{\link{asL1-class}}, \code{\link[distrMod]{asMSE}}, \code{\link{asL4}}}
\examples{
asL1()

## The function is currently defined as
function(biastype = symmetricBias(), normtype = NormType()){ 
         new("asL1", biastype = biastype, normtype = normtype) }
}
\concept{asymptotic mean square error}
\concept{risk}
\keyword{robust}
