\name{asAnscombe-class}
\docType{class}
\alias{asAnscombe-class}
\alias{eff}
\alias{eff,asAnscombe-method}
\alias{show,asAnscombe-method}

\title{Asymptotic Anscombe risk}
\description{Class of asymptotic Anscombe risk which is
  the ARE (asymptotic relative efficiency) in the ideal model
  obtained by an optimal bias robust IC . }
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("asAnscombe", ...)}.
  More frequently they are created via the generating function 
  \code{asAnscombe}. 
}
\section{Slots}{
  \describe{
    \item{\code{type}}{Object of class \code{"character"}: 
      \dQuote{optimal bias robust IC (OBRI) for given ARE (asymptotic relative efficiency)}. }
    \item{\code{eff}}{Object of class \code{"numeric"}: 
      given ARE (asymptotic relative efficiency) to be attained in the ideal model. }
    \item{\code{biastype}}{Object of class \code{"BiasType"}: 
      symmetric, one-sided or asymmetric }
  }
}
\section{Extends}{
Class \code{"asRiskwithBias"}, directly.\cr
Class \code{"asRisk"}, by class \code{"asRiskwithBias"}.
Class \code{"RiskType"}, by class \code{"asRisk"}.
}
\section{Methods}{
  \describe{
    \item{eff}{\code{signature(object = "asAnscombe")}: 
      accessor function for slot \code{eff}. }
    \item{show}{\code{signature(object = "asAnscombe")}}
  }
}
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
%\note{}
\seealso{\code{\link{asRisk-class}}, \code{\link{asAnscombe}}}
\examples{
new("asAnscombe")
}
\concept{Hampel risk}
\concept{risk}
\keyword{classes}
