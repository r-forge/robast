\name{comparePlot-methods}
\docType{methods}
\alias{comparePlot}
\alias{comparePlot-methods}
\alias{comparePlot,IC,IC-method}

\title{Compare - Plots}
\description{
  Plots 2-4 influence curves to the same model.
}
\details{
S4-Method \code{comparePlot} for signature \code{IC,IC} has been enhanced compared to
its original definition in \pkg{RobAStBase} so that if
argument \code{MBRB} is \code{NA}, it is filled automatically by a call
to \code{optIC} which computes the MBR-IC on the fly. To this end, there
is an additional argument \code{n.MBR} defaulting to 10000
to determine the number of evaluation points.
}
\examples{
## all (interesting) examples to this function need
## more time than 5 seconds;
## you can find them in 
## system.file("scripts", "examples_taking_longer.R", 
##              package="ROptEst")
}
\keyword{robust}
