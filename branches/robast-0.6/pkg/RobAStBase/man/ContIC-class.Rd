\name{ContIC-class}
\docType{class}
\alias{ContIC-class}
\alias{CallL2Fam<-,ContIC-method}
\alias{cent}
\alias{cent,ContIC-method}
\alias{cent<-}
\alias{cent<-,ContIC-method}
\alias{clip}
\alias{clip,ContIC-method}
\alias{clip<-}
\alias{clip<-,ContIC-method}
\alias{lowerCase<-}
\alias{lowerCase<-,ContIC-method}
\alias{stand<-}
\alias{stand<-,ContIC-method}
\alias{neighbor,ContIC-method}
\alias{generateIC,ContNeighborhood,L2ParamFamily-method}
\alias{show,ContIC-method}

\title{Influence curve of contamination type}
\description{Class of (partial) influence curves of contamination type; 
  i.e., influence curves \eqn{\eta}{eta} of the form
  \deqn{\eta = (A\Lambda - a)\min(1,b/|A\Lambda - a|)}{eta = (A Lambda - a)min(1, b/|A Lambda - a|)}
  with clipping bound \eqn{b}, centering constant \eqn{a} and
  standardizing matrix \eqn{A}. \eqn{\Lambda}{Lambda} stands for
  the L2 derivative of the corresponding L2 differentiable 
  parametric family created via the call in the slot \code{CallL2Fam}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{new("ContIC", ...)}.
  More frequently they are created via the generating function 
  \code{ContIC}, respectively via the method \code{generateIC}. 
}
\section{Slots}{
  \describe{
    \item{\code{CallL2Fam}:}{ object of class \code{"call"}:
      creates an object of the underlying L2-differentiable 
      parametric family. }

    \item{\code{name}:}{ object of class \code{"character"} }

    \item{\code{Curve}:}{ object of class \code{"EuclRandVarList"}}

    \item{\code{modifyIC}:}{ Object of class \code{"OptionalFunction"}:
      function of two arguments, which are an L2 parametric family 
      and an optional influence curve. Returns an object of 
      class \code{"IC"}. This slot is mainly used for internal
      computations! }

    \item{\code{Risks}:}{ object of class \code{"list"}: 
      list of risks; cf. \code{\link[distrMod]{RiskType-class}}. }

    \item{\code{Infos}:}{ object of class \code{"matrix"}
      with two columns named \code{method} and \code{message}:
      additional informations. }

    \item{\code{clip}:}{ object of class \code{"numeric"}: 
      clipping bound. }

    \item{\code{cent}:}{ object of class \code{"numeric"}:
      centering constant. }

    \item{\code{stand}:}{ object of class \code{"matrix"}:
      standardizing matrix. }

    \item{\code{weight}:}{ object of class \code{"HampelWeight"}:
      weight function }

    \item{\code{biastype}:}{ object of class \code{"BiasType"}:
      bias type (symmetric/onsided/asymmetric) }

    \item{\code{normtype}:}{ object of class \code{"NormType"}:
      norm type (Euclidean, information/self-standardized)}

    \item{\code{lowerCase}:}{ object of class \code{"OptionalNumeric"}:
      optional constant for lower case solution. }

    \item{\code{neighborRadius}:}{ object of class \code{"numeric"}:
      radius of the corresponding (unconditional) contamination 
      neighborhood. }
  }
}
\section{Extends}{
Class \code{"HampIC"}, directly.\cr
Class \code{"IC"}, by class \code{"HampIC"}.\cr
Class \code{"InfluenceCurve"}, by class \code{"IC"}.
}
\section{Methods}{
  \describe{
    \item{CallL2Fam<-}{\code{signature(object = "ContIC")}: 
      replacement function for slot \code{CallL2Fam}. }

    \item{cent}{\code{signature(object = "ContIC")}: 
      accessor function for slot \code{cent}. }

    \item{cent<-}{\code{signature(object = "ContIC")}: 
      replacement function for slot \code{cent}. }

    \item{clip}{\code{signature(object = "ContIC")}: 
      accessor function for slot \code{clip}. }

    \item{clip<-}{\code{signature(object = "ContIC")}: 
      replacement function for slot \code{clip}. }

    \item{stand<-}{\code{signature(object = "ContIC")}:
      replacement function for slot \code{stand}. }

    \item{lowerCase<-}{\code{signature(object = "ContIC")}: 
      replacement function for slot \code{lowerCase}. }

    \item{neighbor}{\code{signature(object = "ContIC")}: 
      generates an object of class \code{"ContNeighborhood"} with
      radius given in slot \code{neighborRadius}. }

    \item{generateIC}{\code{signature(neighbor = "ContNeighborhood", L2Fam = "L2ParamFamily")}:
      generate an object of class \code{"ContIC"}. Rarely called directly. }

    \item{show}{\code{signature(object = "ContIC")}}
  }
}
\references{
  Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.

  Kohl, M. (2005) \emph{Numerical Contributions to the Asymptotic Theory of Robustness}. 
  Bayreuth: Dissertation.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de}}
%\note{}
\seealso{\code{\link{IC-class}}, \code{\link{ContIC}} \code{\link{HampIC-class}}}
\examples{
IC1 <- new("ContIC")
plot(IC1)
}
\concept{influence curve}
\keyword{classes}
