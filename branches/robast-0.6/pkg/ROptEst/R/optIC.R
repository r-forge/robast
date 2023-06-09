###############################################################################
## Optimally robust IC for infinitesimal robust model and asymptotic risks
###############################################################################
setMethod("optIC", signature(model = "InfRobModel", risk = "asRisk"),
    function(model, risk, z.start = NULL, A.start = NULL, upper = 1e4, 
             maxiter = 50, tol = .Machine$double.eps^0.4, warn = TRUE, 
             noLow = FALSE, verbose = FALSE){
        L2derivDim <- numberOfMaps(model@center@L2deriv)
        if(L2derivDim == 1){
            ow <- options("warn")
            options(warn = -1)
            res <- getInfRobIC(L2deriv = model@center@L2derivDistr[[1]], 
                        neighbor = model@neighbor, risk = risk, 
                        symm = model@center@L2derivDistrSymm[[1]],
                        Finfo = model@center@FisherInfo, trafo = model@center@param@trafo, 
                        upper = upper, maxiter = maxiter, tol = tol, warn = warn,
                        noLow = noLow, verbose = verbose)
            options(ow)
            res$info <- c("optIC", res$info)
            res <- c(res, modifyIC = getModifyIC(L2FamIC = model@center, 
                                                 neighbor = model@neighbor, 
                                                 risk = risk))
            return(generateIC(model@neighbor, model@center, res))
        }else{
            if(is(model@center@distribution, "UnivariateDistribution")){
                if((length(model@center@L2deriv) == 1) & is(model@center@L2deriv[[1]], "RealRandVariable")){
                    L2deriv <- model@center@L2deriv[[1]]
                    L2derivSymm <- model@center@L2derivSymm
                    L2derivDistrSymm <- model@center@L2derivDistrSymm
                }else{
                    L2deriv <- diag(dimension(model@center@L2deriv)) %*% model@center@L2deriv
                    L2deriv <- RealRandVariable(Map = L2deriv@Map, Domain = L2deriv@Domain)
                    nrvalues <- numberOfMaps(L2deriv)
                    if(numberOfMaps(model@center@L2deriv) != nrvalues){
                        L1 <- vector("list", nrvalues)
                        L2 <- vector("list", nrvalues)
                        for(i in 1:nrvalues){
                            L1[[i]] <- NonSymmetric()
                            L2[[i]] <- NoSymmetry()
                        }
                        L2derivSymm <- new("FunSymmList", L1)
                        L2derivDistrSymm <- new("DistrSymmList", L2)
                    }
                }
                ow <- options("warn")
                options(warn = -1)
                res <- getInfRobIC(L2deriv = L2deriv, neighbor = model@neighbor, 
                            risk = risk,  Distr = model@center@distribution, 
                            DistrSymm = model@center@distrSymm, L2derivSymm = L2derivSymm,
                            L2derivDistrSymm = L2derivDistrSymm, Finfo = model@center@FisherInfo, 
                            trafo = model@center@param@trafo, z.start = z.start, A.start = A.start, 
                            upper = upper, maxiter = maxiter, tol = tol, warn = warn, 
                            verbose = verbose)
                options(ow)
                res$info <- c("optIC", res$info)
                res <- c(res, modifyIC = getModifyIC(L2FamIC = model@center, 
                                                     neighbor = model@neighbor, 
                                                     risk = risk))
                    return(generateIC(model@neighbor, model@center, res))
                }else{
                stop("not yet implemented")
            }
        }
    })

###############################################################################
## Optimally robust IC for robust model with fixed neighborhood 
## and asymptotic under-/overshoot risk
###############################################################################
setMethod("optIC", signature(model = "InfRobModel", risk = "asUnOvShoot"),
    function(model, risk, upper = 1e4, maxiter = 50, 
             tol = .Machine$double.eps^0.4, warn = TRUE){
        L2derivDistr <- model@center@L2derivDistr[[1]]
        if((length(model@center@L2derivDistr) == 1) & is(L2derivDistr, "UnivariateDistribution")){
            if(identical(all.equal(model@neighbor@radius, 0), TRUE)){
               return(optIC(model@center, risk = asCov()))
            }else{
               ow <- options("warn")
               options(warn = -1)
               res <- getInfRobIC(L2deriv = L2derivDistr, 
                        neighbor = model@neighbor, risk = risk, 
                        symm = model@center@L2derivDistrSymm[[1]],
                        Finfo = model@center@FisherInfo, trafo = model@center@param@trafo, 
                        upper = upper, maxiter = maxiter, tol = tol, warn = warn)
               options(ow)
               if(is(model@neighbor, "ContNeighborhood"))
                  res$info <- c("optIC", "optIC", res$info, "Optimal IC for 'InfRobModel' with 'ContNeighborhood'!!!")
               else
                  res$info <- c("optIC", res$info)
               res <- c(res, modifyIC = getModifyIC(L2FamIC = model@center, 
                                                    neighbor = model@neighbor, 
                                                    risk = risk))
               return(generateIC(TotalVarNeighborhood(radius = model@neighbor@radius), model@center, res))
           }    
        }else{
            stop("restricted to 1-dimensional parameteric models")
        }
    })

###############################################################################
## Optimally robust IC for robust model with fixed neighborhood 
## and finite-sample under-/overshoot risk
###############################################################################
setMethod("optIC", signature(model = "FixRobModel", risk = "fiUnOvShoot"),
    function(model, risk, sampleSize, upper = 1e4, maxiter = 50, 
             tol = .Machine$double.eps^0.4, warn = TRUE, Algo = "A", 
             cont = "left", verbose = FALSE){
        if(!identical(all.equal(sampleSize, trunc(sampleSize)), TRUE))
            stop("'sampleSize' has to be an integer > 0")
        if(is(model@center@distribution, "UnivariateDistribution")){
            ow <- options("warn")
            options(warn = -1)
            res <- getFixRobIC(Distr = model@center@distribution, 
                        neighbor = model@neighbor, risk = risk, 
                        sampleSize = sampleSize, upper = upper, maxiter = maxiter, 
                        tol = tol, warn = warn, Algo = Algo, cont = cont)
            options(ow)
            if(is(model@neighbor, "ContNeighborhood"))
                res$info <- c("optIC", "optIC", res$info, "Optimal IC for 'FixRobModel' with 'ContNeighborhood'!!!")
            else
                res$info <- c("optIC", res$info)
            res <- c(res, modifyIC = getModifyIC(L2FamIC = model@center, 
                                                 neighbor = model@neighbor, 
                                                 risk = risk))
            return(generateIC(TotalVarNeighborhood(radius = model@neighbor@radius), model@center, res))
        }else{
            stop("restricted to 1-dimensional parametric models")
        }
    })
