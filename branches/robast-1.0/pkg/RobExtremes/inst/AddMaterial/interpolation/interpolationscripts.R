#############################################################################
# just creating csv files
#############################################################################
## install new versions of distr-family and robast-family of pkgs
### open R session
require(RobExtremes)
### -> change this according to where you checked out the svn repo:
#.basepath <- "C:/rtest/RobASt/branches/robast-1.0./pkg"
.basepath <- "/p/fm/PeterRuckdeschel/rtest/RobASt/branches/robast-1.0/pkg"
.myFolderTo <- file.path(.basepath,"RobExtremesBuffer")
## <-
oldwd <- getwd()
setwd(.myFolderTo)
.OMSE.th <- ROptEst:::.OMSE.th
.MBRE.th <- ROptEst:::.MBRE.th
.RMXE.th <- ROptEst:::.RMXE.th
.modify.xi.PFam.call <- RobExtremes:::.modify.xi.PFam.call
#
###
xiGridpos <- getShapeGrid(700, cutoff.at.0=0.005)
lxipos <- length(xiGridpos)
(lxipos1 <- 1:(lxipos%/%4))
(lxipos2 <- (1:(lxipos%/%4))+lxipos%/%4)
(lxipos3 <- (1:(lxipos%/%4))+2*lxipos%/%4)
(lxipos4 <- (1:lxipos)[-c(1:(3*(lxipos%/%4)))])
xiGridpos1 <- xiGridpos[lxipos1]
xiGridpos2 <- xiGridpos[lxipos2]
xiGridpos3 <- xiGridpos[lxipos3]
xiGridpos4 <- xiGridpos[lxipos4]
xiGridneg <- seq(-1/2+0.005,-0.005,length=150)

.svInt <- RobExtremes:::.svInt
###
#
## to make this parallel, start this on several processors
#
# compute the interpolation grid of Lagrange multiplier values
#   (still not as interpolators, still not yet smoothed)
#
### Block01--Block15: GEVFamilyMuUnknown
#    Block01--Block05: RMXE, (pos1, pos2, pos3, pos4, neg)
#    Block06--Block10: OMSE, (pos1, pos2, pos3, pos4, neg)
#    Block11--Block15: MBRE, (pos1, pos2, pos3, pos4, neg)
##
### Block16--Block18: GEVFamily, negative xi: RMXE, OMSE, MBRE
#
### Block18--Block21: GParetoFamily, negative xi: RMXE, OMSE, MBRE
#
# in the end, the results are stored in files like
#   interpol.OMSEpos1GEVUFamily.csv in
#
if(FALSE){
  ## Block01::      interpol.RMXEpos1GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridpos1,namFzus="pos1")
}
if(FALSE){
  ## Block02::      interpol.RMXEpos2GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridpos2,namFzus="pos2")
}
if(FALSE){
  ## Block03::      interpol.RMXEpos3GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridpos3,namFzus="pos3")
}
if(FALSE){
  ## Block04::      interpol.RMXEpos4GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridpos4,namFzus="pos4")
}
if(FALSE){
  ## Block05::      interpol.RMXEnegGEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridneg,namFzus="neg") ###Problem
}
if(FALSE){
  ## Block06::      interpol.OMSEpos1GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridpos1,namFzus="pos1")
}
if(FALSE){
  ## Block07::      interpol.OMSEpos2GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridpos2,namFzus="pos2")
}
if(FALSE){
  ## Block08::      interpol.OMSEpos3GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridpos4,namFzus="pos3")  ### 16.12.14 erledigt...
}
if(FALSE){
  ## Block09::      interpol.OMSEpos4GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridpos3,namFzus="pos4")
}
if(FALSE){
  ## Block10::      interpol.OMSEnegGEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridneg,namFzus="neg") ## Problem
}
if(FALSE){
  ## Block11::      interpol.MBREpos1GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridpos1,namFzus="pos1")
}
if(FALSE){
  ## Block12::      interpol.MBREpos2GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridpos2,namFzus="pos2")
}
if(FALSE){
  ## Block13::      interpol.MBREpos3GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridpos3,namFzus="pos3")
}
if(FALSE){
  ## Block14::      interpol.MBREpos4GEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridpos4,namFzus="pos4")
}
if(FALSE){
  ## Block15::      interpol.MBREnegGEVUFamily.csv
   PF <- GEVFamilyMuUnknown(withPos=FALSE, ..name="GEVU Family")
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridneg,namFzus="neg") ### Problem
}
if(FALSE){
  ## Block16::      interpol.RMXEnegGEVFamily.csv
   PF <- GEVFamily(withPos=FALSE)
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg") ### Problem
}                                                           ####erledigt 16.12.
if(FALSE){
  ## Block17::      interpol.OMSEnegGEVFamily.csv
   PF <- GEVFamily(withPos=FALSE)
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg")
}
if(FALSE){
  ## Block18::      interpol.MBREnegGEVFamily.csv
   PF <- GEVFamily(withPos=FALSE)
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg")
}
if(FALSE){
  ## Block19::      interpol.RMXEnegGParetoFamily.csv
   PF <- GParetoFamily(withPos=FALSE)
  .svInt(.RMXE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg")
}
if(FALSE){
  ## Block20::      interpol.OMSEnegGParetoFamily.csv
   PF <- GParetoFamily(withPos=FALSE)
  .svInt(.OMSE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg")
}
if(FALSE){
  ## Block21::      interpol.MBREnegGParetoFamily.csv
   PF <- GParetoFamily(withPos=FALSE)
  .svInt(.MBRE.th, PFam=PF, xiGrid = xiGridneg, namFzus="neg")
}


setwd(oldwd)
