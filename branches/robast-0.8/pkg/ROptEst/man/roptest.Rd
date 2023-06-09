\name{roptest}
\alias{roptest}
\title{ Optimally robust estimation }
\description{
  Function to compute optimally robust estimates for L2-differentiable
  parametric families via k-step construction.
}
\usage{
roptest(x, L2Fam, eps, eps.lower, eps.upper, fsCor = 1, initial.est, 
        neighbor = ContNeighborhood(), risk = asMSE(), steps = 1L, 
        distance = CvMDist, startPar = NULL, verbose = NULL,
        OptOrIter = "iterate",
        useLast = getRobAStBaseOption("kStepUseLast"),
        withUpdateInKer = getRobAStBaseOption("withUpdateInKer"),
        IC.UpdateInKer = getRobAStBaseOption("IC.UpdateInKer"),
        withICList = getRobAStBaseOption("withICList"),
        withPICList = getRobAStBaseOption("withPICList"),
        na.rm = TRUE, initial.est.ArgList, ...)
}
\arguments{
  \item{x}{ sample }
  \item{L2Fam}{ object of class \code{"L2ParamFamily"} }
  \item{eps}{ positive real (0 < \code{eps} <= 0.5): amount of gross errors. 
          See details below. }
  \item{eps.lower}{ positive real (0 <= \code{eps.lower} <= \code{eps.upper}): 
        lower bound for the amount of gross errors. See details below. }
  \item{eps.upper}{ positive real (\code{eps.lower} <= \code{eps.upper} <= 0.5): 
        upper bound for the amount of gross errors. See details below. }
  \item{fsCor}{ positive real: factor used to correct the neighborhood radius;
        see details. }
  \item{initial.est}{ initial estimate for unknown parameter. If missing 
        minimum distance estimator is computed. }
  \item{neighbor}{ object of class \code{"UncondNeighborhood"} }
  \item{risk}{ object of class \code{"RiskType"} }
  \item{steps}{ positive integer: number of steps used for k-steps construction }
  \item{distance}{ distance function }
  \item{startPar}{ initial information used by \code{optimize} resp. \code{optim};
    i.e; if (total) parameter is of length 1, \code{startPar} is 
    a search interval, else it is an initial parameter value; if \code{NULL}
    slot \code{startPar} of \code{ParamFamily} is used to produce it;
    in the multivariate case, \code{startPar} may also be of class \code{Estimate},
    in which case slot \code{untransformed.estimate} is used.}
  \item{verbose}{ logical: if \code{TRUE}, some messages are printed }
  \item{useLast}{ which parameter estimate (initial estimate or
    k-step estimate) shall be used to fill the slots \code{pIC},
    \code{asvar} and \code{asbias} of the return value. }
  \item{OptOrIter}{character; which method to be used for determining Lagrange
  multipliers \code{A} and \code{a}: if (partially) matched to \code{"optimize"},
  \code{getLagrangeMultByOptim} is used; otherwise: by default, or if matched to
  \code{"iterate"} or to \code{"doubleiterate"},
  \code{getLagrangeMultByIter} is used. More specifically,
  when using \code{getLagrangeMultByIter}, and if argument \code{risk} is of
  class \code{"asGRisk"}, by default and if matched to \code{"iterate"}
  we use only one (inner) iteration, if matched to \code{"doubleiterate"}
  we use up to \code{Maxiter} (inner) iterations.}
  \item{withUpdateInKer}{if there is a non-trivial trafo in the model with matrix \eqn{D}, shall
  the parameter be updated on \eqn{{\rm ker}(D)}{ker(D)}?}
  \item{IC.UpdateInKer}{if there is a non-trivial trafo in the model with matrix \eqn{D},
  the IC to be used for this; if \code{NULL} the result of \code{getboundedIC(L2Fam,D)} is taken;
  this IC will then be projected onto \eqn{{\rm ker}(D)}{ker(D)}.}
  \item{withPICList}{logical: shall slot \code{pICList} of return value
  be filled?}
  \item{withICList}{logical: shall slot \code{ICList} of return value
  be filled?}
  \item{na.rm}{logical: if  \code{TRUE}, the estimator is evaluated at \code{complete.cases(x)}.}
  \item{initial.est.ArgList}{a list of arguments to be given to argument \code{start} if the latter
   is a function; this list by default already starts with two unnamed items,
   the sample \code{x}, and the model \code{L2Fam}.}
  \item{\dots}{ further arguments }
}
\details{
  Computes the optimally robust estimator for a given L2 differentiable
  parametric family. The computation uses a k-step construction with an 
  appropriate initial estimate; cf. also \code{\link[RobAStBase]{kStepEstimator}}. 
  Valid candidates are e.g. Kolmogorov(-Smirnov) or von Mises minimum 
  distance estimators (default); cf. Rieder (1994) and Kohl (2005).

  If the amount of gross errors (contamination) is known, it can be 
  specified by \code{eps}. The radius of the corresponding infinitesimal 
  contamination neighborhood is obtained by multiplying \code{eps} 
  by the square root of the sample size. 

  If the amount of gross errors (contamination) is unknown, try to find a 
  rough estimate for the amount of gross errors, such that it lies 
  between \code{eps.lower} and \code{eps.upper}.

  In case \code{eps.lower} is specified and \code{eps.upper} is missing, 
  \code{eps.upper} is set to 0.5. In case \code{eps.upper} is specified and
  \code{eps.lower} is missing, \code{eps.lower} is set to 0.

  If neither \code{eps} nor \code{eps.lower} and/or \code{eps.upper} is 
  specified, \code{eps.lower} and \code{eps.upper} are set to 0 and 0.5, 
  respectively.

  If \code{eps} is missing, the radius-minimax estimator in sense of 
  Rieder et al. (2001, 2008), respectively Section 2.2 of Kohl (2005) is returned.

  Finite-sample and higher order results suggest that the asymptotically
  optimal procedure is to liberal. Using \code{fsCor} the radius can be
  modified - as a rule enlarged - to obtain a more conservative estimate.
  In case of normal location and scale there is function 
  \code{\link[RobLox]{finiteSampleCorrection}} which returns a finite-sample 
  corrected (enlarged) radius based on the results of large Monte-Carlo
  studies.

  The default value of argument \code{useLast} is set by the
  global option \code{kStepUseLast} which by default is set to 
  \code{FALSE}. In case of general models \code{useLast} 
  remains unchanged during the computations. However, if 
  slot \code{CallL2Fam} of \code{IC} generates an object of 
  class \code{"L2GroupParamFamily"} the value of \code{useLast} 
  is changed to \code{TRUE}.
  Explicitly setting \code{useLast} to \code{TRUE} should
  be done with care as in this situation the influence curve
  is re-computed using the value of the one-step estimate
  which may take quite a long time depending on the model.

  If \code{useLast} is set to \code{TRUE} the computation of \code{asvar}, 
  \code{asbias} and \code{IC} is based on the k-step estimate.
}
\value{Object of class \code{"kStepEstimate"}. }
\references{
  Kohl, M. (2005) \emph{Numerical Contributions to the Asymptotic Theory of Robustness}. 
  Bayreuth: Dissertation.

  Kohl, M. and Ruckdeschel, P. (2010): R package distrMod: 
  Object-Oriented Implementation of Probability Models. 
  J. Statist. Softw.  \bold{35}(10), 1--27
  
  Kohl, M. and Ruckdeschel, P., and Rieder, H. (2010):
  Infinitesimally Robust Estimation in General Smoothly Parametrized Models.
  \emph{Stat. Methods Appl.}, \bold{19}, 333--354. 

 Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.

  Rieder, H., Kohl, M. and Ruckdeschel, P. (2008) The Costs of not Knowing
  the Radius. Statistical Methods and Applications \bold{17}(1) 13-40.

  Rieder, H., Kohl, M. and Ruckdeschel, P. (2001) The Costs of not Knowing
  the Radius. Appeared as discussion paper Nr. 81. 
  SFB 373 (Quantification and Simulation of Economic Processes),
  Humboldt University, Berlin; also available under
  \url{www.uni-bayreuth.de/departments/math/org/mathe7/RIEDER/pubs/RR.pdf}
}
\author{Matthias Kohl \email{Matthias.Kohl@stamats.de},\cr
        Peter Ruckdeschel \email{Peter.Ruckdeschel@itwm.fraunhofer.de}}
%\note{}
\seealso{ \code{\link[RobLox]{roblox}}, 
          \code{\link[distrMod]{L2ParamFamily-class}}
          \code{\link[RobAStBase]{UncondNeighborhood-class}},
          \code{\link[distrMod]{RiskType-class}} }
\examples{
#############################
## 1. Binomial data
#############################
## generate a sample of contaminated data
ind <- rbinom(100, size=1, prob=0.05) 
x <- rbinom(100, size=25, prob=(1-ind)*0.25 + ind*0.9)

## ML-estimate
MLest <- MLEstimator(x, BinomFamily(size = 25))
estimate(MLest)
confint(MLest)

## compute optimally robust estimator (known contamination)
robest1 <- roptest(x, BinomFamily(size = 25), eps = 0.05, steps = 3)
estimate(robest1)
confint(robest1, method = symmetricBias())
## neglecting bias
confint(robest1)
plot(pIC(robest1))
qqplot(x, robest1, cex.pch=1.5, exp.cex2.pch = -.25,
       exp.fadcol.pch = .55, jit.fac=.9)

## compute optimally robust estimator (unknown contamination)
robest2 <- roptest(x, BinomFamily(size = 25), eps.lower = 0, eps.upper = 0.2, steps = 3)
estimate(robest2)
confint(robest2, method = symmetricBias())
plot(pIC(robest2))

## total variation neighborhoods (known deviation)
robest3 <- roptest(x, BinomFamily(size = 25), eps = 0.025, 
                   neighbor = TotalVarNeighborhood(), steps = 3)
estimate(robest3)
confint(robest3, method = symmetricBias())
plot(pIC(robest3))

## total variation neighborhoods (unknown deviation)
robest4 <- roptest(x, BinomFamily(size = 25), eps.lower = 0, eps.upper = 0.1, 
                   neighbor = TotalVarNeighborhood(), steps = 3)
estimate(robest4)
confint(robest4, method = symmetricBias())
plot(pIC(robest4))


#############################
## 2. Poisson data
#############################
## Example: Rutherford-Geiger (1910); cf. Feller~(1968), Section VI.7 (a)
x <- c(rep(0, 57), rep(1, 203), rep(2, 383), rep(3, 525), rep(4, 532), 
       rep(5, 408), rep(6, 273), rep(7, 139), rep(8, 45), rep(9, 27), 
       rep(10, 10), rep(11, 4), rep(12, 0), rep(13, 1), rep(14, 1))

## ML-estimate
MLest <- MLEstimator(x, PoisFamily())
estimate(MLest)
confint(MLest)

## compute optimally robust estimator (unknown contamination)
robest <- roptest(x, PoisFamily(), eps.upper = 0.1, steps = 3)
estimate(robest)
confint(robest, symmetricBias())
plot(pIC(robest))
qqplot(x, robest, cex.pch=1.5, exp.cex2.pch = -.25,
       exp.fadcol.pch = .55, jit.fac=.9)
 
## total variation neighborhoods (unknown deviation)
robest1 <- roptest(x, PoisFamily(), eps.upper = 0.05, 
                  neighbor = TotalVarNeighborhood(), steps = 3)
estimate(robest1)
confint(robest1, symmetricBias())
plot(pIC(robest1))


#############################
## 3. Normal (Gaussian) location and scale
#############################
## 24 determinations of copper in wholemeal flour
library(MASS)
data(chem)
plot(chem, main = "copper in wholemeal flour", pch = 20)

## ML-estimate
MLest <- MLEstimator(chem, NormLocationScaleFamily())
estimate(MLest)
confint(MLest)

## compute optimally robust estimator (known contamination)
## takes some time -> you can use package RobLox for normal 
## location and scale which is optimized for speed
robest <- roptest(chem, NormLocationScaleFamily(), eps = 0.05, steps = 3)
estimate(robest)
confint(robest, symmetricBias())
plot(pIC(robest))
## plot of relative and absolute information; cf. Kohl (2005)
infoPlot(pIC(robest))

qqplot(chem, robest, cex.pch=1.5, exp.cex2.pch = -.25,
       exp.fadcol.pch = .55, withLab = TRUE, which.Order=1:4,
       exp.cex2.lbl = .12,exp.fadcol.lbl = .45,
       nosym.pCI = TRUE, adj.lbl=c(1.7,.2),
       exact.pCI = FALSE, log ="xy")

## finite-sample correction
if(require(RobLox)){
    n <- length(chem)
    r <- 0.05*sqrt(n)
    r.fi <- finiteSampleCorrection(n = n, r = r)
    fsCor <- r.fi/r
    robest <- roptest(chem, NormLocationScaleFamily(), eps = 0.05, 
                      fsCor = fsCor, steps = 3)
    estimate(robest)
}

## compute optimally robust estimator (unknown contamination)
## takes some time -> use package RobLox!
robest1 <- roptest(chem, NormLocationScaleFamily(), eps.lower = 0.05, 
                   eps.upper = 0.1, steps = 3)
estimate(robest1)
confint(robest1, symmetricBias())
plot(pIC(robest1))
## plot of relative and absolute information; cf. Kohl (2005)
infoPlot(pIC(robest1))
}
\concept{k-step construction}
\concept{optimally robust estimation}
\keyword{robust}
