\name{inputGenerators}
\alias{inputGenerators}
\alias{gennbCtrl}
\alias{genstartCtrl}
\alias{genkStepCtrl}
\title{Input generating functions for function 'robest'}
\description{
  Generating functions to generate structured input for function \code{robest}.
}
\usage{
genkStepCtrl(useLast = getRobAStBaseOption("kStepUseLast"),
                    withUpdateInKer = getRobAStBaseOption("withUpdateInKer"),
                    IC.UpdateInKer = getRobAStBaseOption("IC.UpdateInKer"),
                    withICList = getRobAStBaseOption("withICList"),
                    withPICList = getRobAStBaseOption("withPICList"),
                    scalename = "scale", withLogScale = TRUE)
genstartCtrl(initial.est = NULL, initial.est.ArgList = NULL,
                        startPar = NULL, distance = CvMDist)
gennbCtrl(neighbor = ContNeighborhood(), eps, eps.lower, eps.upper)
}
\arguments{
  \item{useLast}{ which parameter estimate (initial estimate or
    k-step estimate) shall be used to fill the slots \code{pIC},
    \code{asvar} and \code{asbias} of the return value. }
  \item{withUpdateInKer}{if there is a non-trivial trafo in the model with matrix \eqn{D}, shall
  the parameter be updated on \eqn{{\rm ker}(D)}{ker(D)}?}
  \item{IC.UpdateInKer}{if there is a non-trivial trafo in the model with matrix \eqn{D},
  the IC to be used for this; if \code{NULL} the result of \code{getboundedIC(L2Fam,D)} is taken;
  this IC will then be projected onto \eqn{{\rm ker}(D)}{ker(D)}.}
  \item{withICList}{logical: shall slot \code{ICList} of return value
  be filled?}
  \item{withPICList}{logical: shall slot \code{pICList} of return value
  be filled?}
  \item{scalename}{character: name of the respective scale component.}
  \item{withLogScale}{logical; shall a scale component (if existing and found
   with name \code{scalename}) be computed on log-scale and backtransformed
   afterwards? This avoids crossing 0.}

  \item{initial.est}{ initial estimate for unknown parameter. If missing
        minimum distance estimator is computed. }
  \item{initial.est.ArgList}{a list of arguments to be given to argument \code{start} if the latter
   is a function; this list by default already starts with two unnamed items,
   the sample \code{x}, and the model \code{L2Fam}.}
  \item{startPar}{ initial information used by \code{optimize} resp. \code{optim};
    i.e; if (total) parameter is of length 1, \code{startPar} is
    a search interval, else it is an initial parameter value; if \code{NULL}
    slot \code{startPar} of \code{ParamFamily} is used to produce it;
    in the multivariate case, \code{startPar} may also be of class \code{Estimate},
    in which case slot \code{untransformed.estimate} is used.}
  \item{distance}{ distance function }


  \item{neighbor}{ object of class \code{"UncondNeighborhood"} }
  \item{eps}{ positive real (0 < \code{eps} <= 0.5): amount of gross errors.
          See details below. }
  \item{eps.lower}{ positive real (0 <= \code{eps.lower} <= \code{eps.upper}):
        lower bound for the amount of gross errors. See details below. }
  \item{eps.upper}{ positive real (\code{eps.lower} <= \code{eps.upper} <= 0.5):
        upper bound for the amount of gross errors. See details below. }
}
\details{
  All these functions bundle their respective input to (reusable) lists
  which can be used as arguments in function \code{\link{robest}}.
  For details, see this function.
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{ \code{\link[RobLox]{roblox}},
          \code{\link[distrMod]{L2ParamFamily-class}}
          \code{\link[RobAStBase]{UncondNeighborhood-class}},
          \code{\link[distrMod]{RiskType-class}} }
\examples{
genkStepCtrl()
genstartCtrl()
gennbCtrl()
}
\concept{k-step construction}
\concept{optimally robust estimation}
\keyword{robust}