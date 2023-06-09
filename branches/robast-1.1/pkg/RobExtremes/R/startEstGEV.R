.getXiGrid <- function(){c(1, -0.48,0,3,-0.1,0.4 ,0.1,6)}


.getBetaXiGEV <- function(x, mu, xiGrid = .getXiGrid(), withPos=TRUE, secLevel = 0.7,
                          .issueIntermediateParams = FALSE, withMDE = FALSE){

  n <- length(x)
  epsn <- min(floor(secLevel*sqrt(n))+1,n)

  x0 <- x-mu
  s0 <- max(x0)-min(x0)
  crit0 <- Inf

  fu <- function(x,...) .getBetaXiGEV(x,0,xiGrid = xiGrid,withPos=withPos)
  e0 <- NULL
  es <- c(NA,NA)
  
  ### first try (to ensure global consistency): PickandsEstimator
  try({mygev <- GEVFamily(loc=0,scale=1,shape=0.1, withPos=withPos,
                     ..withWarningGEV=FALSE)
       e1 <- PickandsEstimator(x0,ParamFamily=mygev)
       if(.issueIntermediateParams){
           cat("Pickands:\n");print(e1) }
       e0 <- estimate(e1)}, silent=TRUE)

  validi <- 0
  es0 <- c(NA,NA)
  if(!is.null(e0)) if(!is(e0,"try-error")){
      if(!withMDE) {
         names(e0) <- c("scale","shape")
         return(e0)
      }
      mygev <- GEVFamily(loc=0,scale=e0[1],shape=e0[2], withPos=withPos,
                         start0Est = fu, ..withWarningGEV=FALSE)
      mde0 <- try(MDEstimator(x0, mygev, distance=CvMDist, startPar=c("scale"=e0[1],"shape"=e0[2])),silent=TRUE)
      if(!is(mde0,"try-error")){
          es <- estimate(mde0)
          crit1 <- criterion(mde0)
          if(.issueIntermediateParams){
             cat("1st candidate:\n", round(es,6), " crit:", round(crit1,6), , "   ")
          }
          if(quantile(1+es[2]*x0/es[1], epsn/n)>0){
             validi <- 1
             mdeb <- mde0
             crit0 <- crit1
             es0 <- es
             if(.issueIntermediateParams){
                cat("side condition '1+sc/sh (x-mu) >0' fulfilled;\n")
             }
             names(es) <- c("scale","shape")
             return(es)
          }else{
             if(.issueIntermediateParams){
                cat("side condition '1+sc/sh (x-mu) >0' violated;\n")
             }
          }
      }
  }

  i <- 0
  sd0 <- c(Inf,Inf)
  esS <- matrix(NA,length(xiGrid)+validi,2)
  if(validi>0) esS[1,] <- es0

  while((i<length(xiGrid))&&max(sd0)>1e-3){
      i <- i + 1
      xi <- xiGrid[i]
      funl <- function(sig){
         mygev1 <- GEV(loc=0,scale=sig,shape=xi)
         CvMDist(x0,mygev1)
      }
      intlo <- quantile(-xi*x0,1-epsn/n)
      intv <-  c(max(1e-5,intlo), s0)
      sigCvMMD1 <- optimize(funl, interval=intv)$minimum
      mygev <- GEVFamily(loc=0,scale=sigCvMMD1,shape=xi, withPos=withPos,
                         start0Est = fu, ..withWarningGEV=FALSE)
      mde0 <- try(MDEstimator(x0, mygev, distance=CvMDist, startPar=c("scale"=sigCvMMD1,"shape"=xi)),silent=TRUE)
      es0 <- c(NA,NA)
      if(!is(mde0,"try-error")){
          es <- estimate(mde0)
          crit1 <- criterion(mde0)
          if(.issueIntermediateParams){
              cat("candidate no",i+1, ":\n", round(es,6), " crit:", round(crit1,6), "   ")
          }
          if(quantile(1+es[2]*x0/es[1], epsn/n)>0){
             validi <- validi+1
             esS[validi,] <- es
             if(validi>2) sd0 <- apply(esS[1:validi,],2,sd)
             if(.issueIntermediateParams){
                   cat("side condition '1+sc/sh (x-mu) >0' fulfilled;\n")
             }
             if(crit1<crit0){
                mdeb <- mde0
                crit0 <- crit1
                es0 <- es
             }
          }else{
             if(.issueIntermediateParams){
                cat("side condition '1+sc/sh (x-mu) >0' violated;\n")
             }
             es[1] <- intlo+1e-5
             mygev2 <- GEV(loc=0,scale=es[1],shape=es[2])
             crit1 <- CvMDist(x0,mygev2)
             validi <- validi+1
             esS[validi,] <- es
             if(validi>2) sd0 <- apply(esS[1:validi,],2,sd)
             if(.issueIntermediateParams){
                 cat("candidate no",i+1, "(b):\n", round(es,6), " crit:", round(crit1,6), "   ")
             }
             if(crit1<crit0){
                crit0 <- crit1
                es0 <- es
             }
          }
      }
  }
  names(es) <- c("scale","shape")
  return(es)
}

.getMuBetaXiGEV <- function(x, xiGrid = .getXiGrid(), withPos=TRUE, secLevel = 0.7,
                            .issueIntermediateParams = FALSE, withMDE = FALSE){
  mu <- quantile(x,exp(-1))
  es <- .getBetaXiGEV(x=x, mu=mu, xiGrid=xiGrid, withPos=withPos,
                      secLevel = secLevel,
                      .issueIntermediateParams = .issueIntermediateParams,
                      withMDE = withMDE)
  es0 <- c(mu,es)
  names(es0) <- c("loc","scale","shape")
  return(es0)
}
