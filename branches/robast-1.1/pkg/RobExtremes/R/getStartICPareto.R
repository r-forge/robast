setMethod("getStartIC",signature(model = "ParetoFamily", risk = "interpolRisk"),
           function(model, risk, ..., withMakeIC = FALSE){

    param1 <- param(model)
    xi <- main(param1)
    .modifyIC0 <- function(L2Fam, IC){
              xi0 <- main(param(L2Fam))
              return(.getPsi.P(xi0, L2Fam, type(risk), withMakeIC))
    }
    attr(.modifyIC0,"hasMakeICin.modifyIC") <- TRUE
    .modifyIC <- function(L2Fam,IC){
         psi.0 <- .modifyIC0(L2Fam,IC)
         psi.0@modifyIC <- .modifyIC
         return(psi.0)
    }
    attr(.modifyIC,"hasMakeICin.modifyIC") <- TRUE
    IC0 <- .getPsi.P(xi, model, type(risk), withMakeIC)
    IC0@modifyIC <- .modifyIC
    return(IC0)
    })

.getPsi.P <- function(xi, L2Fam, type, withMakeIC){
   ## the respective LMs have been computed ahead of time
   ## and stored in sysdata.rda of this package
   ## the code for this computation is in AddMaterial/getLMPareto.R
   .PLM <- getFromNamespace(".ParetoLM", ns = "RobExtremes")
   if(type==".MBRE"){
         b  <- xi*.PLM$MBR["b"]
         a  <- xi*.PLM$MBR["a"]
         aw <- 1/xi*.PLM$MBR["aw"]
         A  <- matrix(xi^2*.PLM$MBR["A"],1,1)
         Aw <- matrix(xi^2*.PLM$MBR["Aw"],1,1)
   }else{if(type==".RMXE"){
         b  <- xi*.PLM$RMX["b"]
         a  <- xi*.PLM$RMX["a"]
         aw <- 1/xi*.PLM$RMX["aw"]
         A  <- matrix(xi^2*.PLM$RMX["A"],1,1)
         Aw <- matrix(xi^2*.PLM$RMX["Aw"],1,1)
      }else{if(type==".OMSE"){
         b  <- xi*.PLM$OMS["b"]
         a  <- xi*.PLM$OMS["a"]
         aw <- 1/xi*.PLM$OMS["aw"]
         A  <- matrix(xi^2*.PLM$OMS["A"],1,1)
         Aw <- matrix(xi^2*.PLM$OMS["Aw"],1,1)
         }
      }
   }
   normt <- NormType()
   biast <- symmetricBias()
   nb <- ContNeighborhood(radius=0.5)
   ICT <- paste("optimally robust IC for", switch(type,
                      ".OMSE"="maxMSE",".RMXE"="RMX", ".MBRE"="maxBias"))
   riskT <- if(type!=".MBRE") "asGRisk" else "asBias"

   w <- new("HampelWeight")
      stand(w) <- Aw
      cent(w) <- aw
      clip(w) <- b

   if(type!=".MBRE"){
         weight(w) <- getweight(w, neighbor = nb, biastype = biast,
                          normW = normt)
   }else weight(w) <- minbiasweight(w, neighbor = nb, biastype = biast,
                          normW = normt)

   res <- list(a = a, A = A, b = b, d = 0*a,
               normtype = normt, biastype = biast, w = w,
               info = c("optIC", ICT), risk = list(),
               modifyIC = NULL)


   IC <- generateIC(nb, L2Fam, res)
   if(withMakeIC) IC <- makeIC(IC,L2Fam)
   return(IC)
}
