\name{rlOptIC}
\alias{rlOptIC}
\title{Computation of the optimally robust IC for AL estimators}
\description{
  The function \code{rlOptIC} computes the optimally robust IC for 
  AL estimators in case of normal location and (convex) contamination 
  neighborhoods. The definition of these estimators can be found 
  in Rieder (1994) or Kohl (2005), respectively.
}
\usage{
rlOptIC(r, mean = 0, sd = 1, bUp = 1000, computeIC = TRUE)
}
\arguments{
  \item{r}{ non-negative real: neighborhood radius. }
  \item{mean}{ specified mean.}
  \item{sd}{ specified standard deviation.}
  \item{bUp}{ positive real: the upper end point of the 
    interval to be searched for the clipping bound b. }
  \item{computeIC}{ logical: should IC be computed. See details below. }
}
\details{
  If 'computeIC' is 'FALSE' only the Lagrange multipliers 'A', 'a', and
  'b' contained in the optimally robust IC are computed.
}
\value{
  If 'computeIC' is 'TRUE' an object of class \code{"ContIC"} is returned, 
  otherwise a list of Lagrange multipliers
  \item{A}{ standardizing constant }
  \item{a}{ centering constant; always '= 0' is this symmetric setup }
  \item{b}{ optimal clipping bound }
}
\references{ 
  M. Kohl (2005). Numerical Contributions to the Asymptotic Theory of Robustness.
  Dissertation. University of Bayreuth. \url{https://epub.uni-bayreuth.de/id/eprint/839/2/DissMKohl.pdf}.
  
  H. Rieder (1994): Robust Asymptotic Statistics. Springer. \doi{10.1007/978-1-4684-0624-5}
  
  M. Kohl, P. Ruckdeschel, and H. Rieder (2010). Infinitesimally Robust Estimation in 
  General Smoothly Parametrized Models. Statistical Methods and Applications \emph{19}(3): 333-354.
  \doi{10.1007/s10260-010-0133-0}.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link[RobAStBase]{ContIC-class}}, \code{\link{roblox}}}
\examples{
IC1 <- rlOptIC(r = 0.1)
distrExOptions("ErelativeTolerance" = 1e-12)
checkIC(IC1)
distrExOptions("ErelativeTolerance" = .Machine$double.eps^0.25) # default
Risks(IC1)
cent(IC1)
clip(IC1)
stand(IC1)
plot(IC1)
}
\concept{normal location}
\concept{influence curve}
\keyword{robust}
