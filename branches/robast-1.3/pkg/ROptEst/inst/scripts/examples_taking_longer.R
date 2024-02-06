##########################################################################
## examples to package "ROptEst" running longer than 5 sec
##########################################################################

#-----------------------------------------------------------
### example for comparePlot() from comparePlot.Rd
#-----------------------------------------------------------

## this example for comparePlot() will need more time than 
## 5 seconds to run 

N0 <- NormLocationScaleFamily(mean=0, sd=1)
N0.Rob1 <- InfRobModel(center = N0,
           neighbor = ContNeighborhood(radius = 0.5))
IC1 <- optIC(model = N0, risk = asCov())
IC2 <- optIC(model = N0.Rob1, risk = asMSE())

comparePlot(IC1,IC2, withMBR=TRUE)


#------------------------------------------
## example for robest, from robest.Rd
#------------------------------------------

#############################
## 3. Normal (Gaussian) location and scale
#############################

## this example of a two dimensional parameter
## to be estimated will need more time than 
## 5 seconds to run 
## you can find it in 
## system.file("scripts", "examples_taking_longer.R", 
##              package="ROptEst")

## 24 determinations of copper in wholemeal flour
library(MASS)
data(chem)
plot(chem, main = "copper in wholemeal flour", pch = 20)

## Family
NF <- NormLocationScaleFamily()
## ML-estimate
MLest <- MLEstimator(chem, NF)
estimate(MLest)
confint(MLest)

## compute optimally robust estimator (known contamination)
## takes some time -> you can use package RobLox for normal 
## location and scale which is optimized for speed
nb1 <- gennbCtrl(eps = 0.05)
robEst <- robest(chem, NF, nbCtrl = nb1, steps = 3)
estimate.call(robEst)
attr(robEst,"timings")
estimate(robEst)

confint(robEst, symmetricBias())
plot(pIC(robEst))
## plot of relative and absolute information; cf. Kohl (2005)
infoPlot(pIC(robEst))

tmp <- qqplot(chem, robEst, cex.pch=1.5, exp.cex2.pch = -.25,
              exp.fadcol.pch = .55, withLab = TRUE, which.Order=1:4,
              exp.cex2.lbl = .12,exp.fadcol.lbl = .45,
              nosym.pCI = TRUE, adj.lbl=c(1.7,.2),
              exact.pCI = FALSE, log ="xy")
             
## finite-sample correction
if(require(RobLox)){
    n <- length(chem)
    r <- 0.05*sqrt(n)
    r.fi <- finiteSampleCorrection(n = n, r = r)
    fsCor0 <- r.fi/r
    nb1 <- gennbCtrl(eps = 0.05)
    robest <- robest(chem, NF, nbCtrl = nb1, fsCor = fsCor0, steps = 3)
    estimate(robest)
}

## compute optimally robust estimator (unknown contamination)
## takes some time -> use package RobLox!
nb2 <- gennbCtrl(eps.lower = 0.05, eps.upper = 0.1)
robest1 <- robest(chem, NF, nbCtrl = nb2, steps = 3)
estimate(robest1)
confint(robest1, symmetricBias())
plot(pIC(robest1))
## plot of relative and absolute information; cf. Kohl (2005)
infoPlot(pIC(robest1))

#------------------------------------------
## example for roptest, from roptest.Rd
#------------------------------------------

robest.with.roptest <- 
    roptest(chem, NormLocationScaleFamily(), eps = 0.05, steps = 3)

### compare with robEst obtained with robest()

class(robEst)
class(robest.with.roptest)

estimate(robEst)
estimate(robest.with.roptest)

is(robEst, "kStepEstimate")
is(robest.with.roptest, "kStepEstimate")

## only differences in calls  and functional slots, where bodies ...


