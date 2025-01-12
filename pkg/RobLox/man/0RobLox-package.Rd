\name{RobLox-package}
\alias{RobLox-package}
\alias{RobLox}
\docType{package}
\title{
Optimally robust influence curves and estimators for location and scale
}
\description{
Functions for the determination of optimally robust influence curves and 
estimators in case of normal location and/or scale 
(see Chapter 8 in Kohl (2005) <https://epub.uni-bayreuth.de/839/2/DissMKohl.pdf>).
}
\author{Matthias Kohl  \email{matthias.kohl@stamats.de}}
\references{
  M. Kohl (2005). Numerical Contributions to the Asymptotic Theory of Robustness.
  Dissertation. University of Bayreuth. \url{https://epub.uni-bayreuth.de/id/eprint/839/2/DissMKohl.pdf}.
  
  H. Rieder (1994): Robust Asymptotic Statistics. Springer. \doi{10.1007/978-1-4684-0624-5}
  
  H. Rieder, M. Kohl, and P. Ruckdeschel (2008). The Costs of Not Knowing the Radius.
  Statistical Methods and Applications  \emph{17}(1): 13-40. \doi{10.1007/s10260-007-0047-7}
  M. Kohl, P. Ruckdeschel, and H. Rieder (2010). Infinitesimally Robust Estimation in 
  General Smoothly Parametrized Models. Statistical Methods and Applications \emph{19}(3): 333-354.
  \doi{10.1007/s10260-010-0133-0}.
  
  M. Kohl and H.P. Deigner (2010). Preprocessing of gene expression data by 
  optimally robust estimators. BMC Bioinformatics \emph{11}, 583.
  \doi{10.1186/1471-2105-11-583}.
  
  M. Kohl (2012). Bounded influence estimation for regression and scale.
  Statistics, \bold{46}(4): 437-488. \doi{10.1080/02331888.2010.540668}
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
