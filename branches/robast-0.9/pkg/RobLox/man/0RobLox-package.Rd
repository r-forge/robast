\name{RobLox-package}
\alias{RobLox-package}
\alias{RobLox}
\docType{package}
\title{
Optimally robust influence curves and estimators for location and scale
}
\description{
Functions for the determination of optimally robust influence curves and 
estimators in case of normal location and/or scale.
}
\details{
\tabular{ll}{
Package: \tab RobLox \cr
Version: \tab 0.8.2 \cr
Date: \tab 2012-09-12 \cr
Depends: \tab R(>= 2.7.0), stats, lattice, RColorBrewer, Biobase, distr, distrMod, RobAStBase\cr
Suggests: \tab MASS\cr
LazyLoad: \tab yes \cr
ByteCompile: \tab yes \cr
License: \tab LGPL-3 \cr
URL: \tab http://robast.r-forge.r-project.org/\cr
SVNRevision: \tab 519 \cr
}
}
\author{Matthias Kohl  \email{matthias.kohl@stamats.de}}
\references{
  M. Kohl (2005). Numerical Contributions to the Asymptotic Theory of Robustness.
  Dissertation. University of Bayreuth.

  Rieder, H. (1994) \emph{Robust Asymptotic Statistics}. New York: Springer.

  Rieder, H., Kohl, M. and Ruckdeschel, P. (2008). The Costs of not Knowing
  the Radius. \emph{Statistical Methods and Applications} \bold{17}(1) 13-40.
  Extended version: \url{http://www.stamats.de/RRlong.pdf}
  
  M. Kohl, P. Ruckdeschel, and H. Rieder (2010). Infinitesimally Robust Estimation 
  in General Smoothly Parametrized Models. \emph{Statistical Methods and Application}, 
  \bold{19}(3):333-354. 
}
\seealso{ \code{\link[RobAStBase:0RobAStBase-package]{RobAStBase-package}} }
\section{Package versions}{
Note: The first two numbers of package versions do not necessarily reflect
 package-individual development, but rather are chosen for the
 RobAStXXX family as a whole in order to ease updating "depends"
 information.
}
\examples{
library(RobLox)
ind <- rbinom(100, size=1, prob=0.05) 
x <- rnorm(100, mean=ind*3, sd=(1-ind) + ind*9)
roblox(x)

res <- roblox(x, eps.lower = 0.01, eps.upper = 0.1, returnIC = TRUE)
estimate(res)
confint(res)
confint(res, method = symmetricBias())
pIC(res)

## don't run to reduce check time on CRAN
\dontrun{
checkIC(pIC(res))
Risks(pIC(res))
Infos(pIC(res))
plot(pIC(res))
infoPlot(pIC(res))
}

## row-wise application
ind <- rbinom(200, size=1, prob=0.05) 
X <- matrix(rnorm(200, mean=ind*3, sd=(1-ind) + ind*9), nrow = 2)
rowRoblox(X)
}
\keyword{package}
