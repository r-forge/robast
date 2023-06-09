###############################################################################
## radius minimax optimally robust IC 
## for L2ParamFamily and asymptotic risks
###############################################################################
setMethod("leastFavorableRadius", signature(L2Fam = "L2ParamFamily", 
                                            neighbor = "UncondNeighborhood",
                                            risk = "asGRisk"),
    function(L2Fam, neighbor, risk, rho, upRad = 1,
            z.start = NULL, A.start = NULL, upper = 100,
            OptOrIter = "iterate", maxiter = 100,
            tol = .Machine$double.eps^0.4, warn = FALSE, verbose = NULL){
        if(missing(verbose)|| is.null(verbose))
           verbose <- getRobAStBaseOption("all.verbose")
        if(length(rho) != 1)
            stop("'rho' is not of length == 1")
        if((rho <= 0)||(rho >= 1))
            stop("'rho' not in (0,1)")

        biastype <- biastype(risk)
        normtype <- normtype(risk)

        trafo <- trafo(L2Fam@param)
        FI0 <- trafo%*%solve(L2Fam@FisherInfo)%*%t(trafo)
        FI <- solve(FI0)
        if(is(normtype,"InfoNorm") || is(normtype,"SelfNorm") ) 
           {QuadForm(normtype) <- PosSemDefSymmMatrix(FI); 
            normtype(risk) <- normtype}

        L2derivDim <- numberOfMaps(L2Fam@L2deriv)
        if(L2derivDim == 1){
            leastFavFct <- function(r, L2Fam, neighbor, risk, rho, 
                                    upper.b, MaxIter, eps, warn){
                loRad <- r*rho
                upRad <- r/rho
                lower <- ifelse(identical(all.equal(loRad, 0), TRUE), 1e-4, loRad)
                upper <- ifelse(upRad == Inf, 10, upRad)
                ow <- options("warn")
                on.exit(options(ow))
                options(warn = -1)
                if(identical(all.equal(loRad, 0), TRUE)){
                    loRad <- 0
                    loRisk <- 1/as.vector(L2Fam@FisherInfo)
                }else{
                    neighbor@radius <- loRad
                    resLo <- getInfRobIC(L2deriv = L2Fam@L2derivDistr[[1]], neighbor = neighbor, 
                                risk = risk, symm = L2Fam@L2derivDistrSymm[[1]],
                                Finfo = L2Fam@FisherInfo, upper = upper.b,
                                trafo = trafo, maxiter = MaxIter, tol = eps, 
                                warn = warn, verbose = verbose)
                    loRisk <- getAsRisk(risk = risk, L2deriv = L2Fam@L2derivDistr[[1]], 
                                        neighbor = neighbor, biastype = biastype, 
                                        normtype = normtype,
                                        clip = resLo$b, cent = resLo$a, 
                                        stand = resLo$A, trafo = trafo)[[1]]
                }

                if(upRad == Inf){
                    bmin <- getAsRisk(risk = asBias(biastype = biastype), 
                                L2deriv = L2Fam@L2derivDistr[[1]], 
                                neighbor = neighbor, biastype = biastype, 
                                normtype = normtype,
                                trafo = trafo, symm = L2Fam@L2derivSymm[[1]])
                    upRisk <- bmin^2
                }else{
                    neighbor@radius <- upRad
                    resUp <- getInfRobIC(L2deriv = L2Fam@L2derivDistr[[1]], neighbor = neighbor, 
                                risk = risk, symm = L2Fam@L2derivDistrSymm[[1]],
                                Finfo = L2Fam@FisherInfo, upper = upper.b,
                                trafo = trafo, maxiter = MaxIter, tol = eps, 
                                warn = warn, verbose = verbose)
                    upRisk <- getAsRisk(risk = risk, L2deriv = L2Fam@L2derivDistr[[1]], 
                                        neighbor = neighbor, biastype = biastype, 
                                        normtype = normtype,
                                        clip = resUp$b, cent = resUp$a, 
                                        stand = resUp$A, trafo = trafo)[[1]]
                }
                loNorm<- upNorm <- NormType()
                leastFavR <- uniroot(getIneffDiff, lower = lower, upper = upper, 
                                tol = .Machine$double.eps^0.25, L2Fam = L2Fam, neighbor = neighbor, 
                                risk = risk, loRad = loRad, upRad = upRad, loRisk = loRisk, 
                                upRisk = upRisk, upper.b = upper.b, eps = eps, MaxIter = MaxIter, 
                                warn = warn, 
                                loNorm = loNorm, upNorm = upNorm)$root
            options(ow)
                cat("current radius:\t", r, "\tinefficiency:\t", ineff, "\n")
                return(ineff)
            }
            leastFavR <- optimize(leastFavFct, lower = 1e-4, upper = upRad, 
                            tol = .Machine$double.eps^0.25, maximum = TRUE,
                            L2Fam = L2Fam, neighbor = neighbor, risk = risk,
                            rho = rho, upper.b = upper, MaxIter = maxiter, 
                            eps = tol, warn = warn)

            res <- list(rho = rho, leastFavorableRadius = leastFavR$maximum, 
                        ineff = leastFavR$objective)
            names(res)[3] <- paste(class(risk)[1], "-inefficiency", sep="")
            return(res)
        }else{
            if(is(L2Fam@distribution, "UnivariateDistribution")){
                if((length(L2Fam@L2deriv) == 1) & is(L2Fam@L2deriv[[1]], "RealRandVariable")){
                    L2deriv <- L2Fam@L2deriv[[1]]
                    L2derivSymm <- L2Fam@L2derivSymm
                    L2derivDistrSymm <- L2Fam@L2derivDistrSymm
                }else{
                    L2deriv <- diag(dimension(L2Fam@L2deriv)) %*% L2Fam@L2deriv
                    L2deriv <- RealRandVariable(Map = L2deriv@Map, Domain = L2deriv@Domain)
                    nrvalues <- numberOfMaps(L2deriv)
                    if(numberOfMaps(L2Fam@L2deriv) != nrvalues){
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
   
                std <- if(is(normtype,"QFNorm")) 
                       QuadForm(normtype) else diag(nrow(trafo))
   
                leastFavFct <- function(r, L2Fam, neighbor, risk, rho, 
                                        z.start, A.start, upper.b, MaxIter, eps, warn){
                    loRad <- r*rho
                    upRad <- r/rho
                    lower <- ifelse(identical(all.equal(loRad, 0), TRUE), 1e-4, loRad)
                    upper <- ifelse(upRad == Inf, 10, upRad)
                    ow <- options("warn")
                    on.exit(options(ow))
                    options(warn = -1)
                    if(identical(all.equal(loRad, 0), TRUE)){
                        loRad <- 0
                        loRisk <- sum(diag(std%*%FI0))
                        loNorm <- normtype                    
                    }else{
                        neighbor@radius <- loRad
                        resLo <- getInfRobIC(L2deriv = L2deriv, neighbor = neighbor, risk = risk, 
                                    Distr = L2Fam@distribution, DistrSymm = L2Fam@distrSymm, 
                                    L2derivSymm = L2derivSymm, L2derivDistrSymm = L2derivDistrSymm, 
                                    Finfo = L2Fam@FisherInfo, trafo = trafo, z.start = z.start, 
                                    A.start = A.start, upper = upper.b,
                                    OptOrIter = OptOrIter, maxiter = MaxIter,
                                    tol = eps, warn = warn, verbose = verbose)
                        riskLo <- risk
                        normtype(riskLo) <- resLo$normtype
                        loRisk <- getAsRisk(risk = riskLo, L2deriv = L2deriv, neighbor = neighbor, 
                                            biastype = biastype, normtype = normtype,
                                            clip = resLo$b, cent = resLo$a, 
                                            stand = resLo$A, trafo = trafo)[[1]]
                        loNorm <- resLo$normtype                    
                    }

                    if(upRad == Inf){
                        biasR <- getAsRisk(risk = asBias(biastype = biastype(risk), 
                                      normtype = normtype), L2deriv = L2deriv, 
                                      neighbor = neighbor, biastype = biastype, 
                                      normtype = normtype,
                                      Distr = L2Fam@distribution, 
                                      DistrSymm = L2Fam@distrSymm, 
                                      L2derivSymm = L2derivSymm, 
                                      L2derivDistrSymm= L2derivDistrSymm,                                       
                                Finfo = L2Fam@FisherInfo, trafo = trafo,
                                z.start = z.start, A.start = A.start,
                                maxiter = maxiter, tol = tol, 
                                warn = warn, verbose = verbose)
                        bmin <- biasR$asBias
                        upRisk <- bmin^2
                        upNorm <- biasR$normtype
                    }else{
                        neighbor@radius <- upRad
                        resUp <- getInfRobIC(L2deriv = L2deriv, neighbor = neighbor, risk = risk, 
                                    Distr = L2Fam@distribution, DistrSymm = L2Fam@distrSymm, 
                                    L2derivSymm = L2derivSymm, L2derivDistrSymm = L2derivDistrSymm, 
                                    Finfo = L2Fam@FisherInfo, trafo = trafo, z.start = z.start, 
                                    A.start = A.start, upper = upper.b,
                                    OptOrIter = OptOrIter, maxiter = maxiter,
                                    tol = tol, warn = warn, verbose = verbose)
                         riskUp <- risk
                         normtype(riskUp) <- resUp$normtype
                         upRisk <- getAsRisk(risk = riskUp, L2deriv = L2deriv, neighbor = neighbor, 
                                        biastype = biastype, normtype = normtype,
                                        clip = resUp$b, cent = resUp$a, stand = resUp$A, trafo = trafo)[[1]]
                         upNorm <- resUp$normtype                    
                    }
                    leastFavR <- uniroot(getIneffDiff, lower = lower, upper = upper, 
                                    tol = .Machine$double.eps^0.25, L2Fam = L2Fam,
                                    neighbor = neighbor, z.start = z.start,
                                    A.start = A.start, upper.b = upper.b,
                                    risk = risk, 
                                    loRad = loRad, upRad = upRad,
                                    loRisk = loRisk, upRisk = upRisk,
                                    eps = eps, OptOrIter = OptOrIter,
                                    MaxIter = MaxIter, warn = warn,
                                    loNorm = loNorm, upNorm = upNorm)$root
                    options(ow)

                    if(verbose)
                       cat(paste(rep("-",75), sep = "", collapse = ""),"\n")
                    cat("current radius:   ", round(radius,4),
                        "\tinefficiency:   ", round(ineff,4))
                    if(verbose)
                       cat(paste("\n",paste(rep("-",75), sep = "",
                                        collapse = ""),"\n",sep=""))
                    else cat("\n")

                    return(ineff)
                }
                if(is.null(z.start)) z.start <- numeric(L2derivDim)
                if(is.null(A.start)) A.start <- trafo
                leastFavR <- optimize(leastFavFct, lower = 1e-4, upper = upRad, 
                                tol = .Machine$double.eps^0.25, maximum = TRUE,
                                L2Fam = L2Fam, neighbor = neighbor, risk = risk,
                                rho = rho, z.start = z.start, A.start = A.start, 
                                upper.b = upper, MaxIter = maxiter, eps = tol, warn = warn)

                res <- list(rho = rho, leastFavorableRadius = leastFavR$maximum, 
                            ineff = leastFavR$objective)
                names(res)[3] <- paste(class(risk)[1], "-inefficiency", sep="")
                return(res)
            }else{
                stop("not yet implemented")
            }
        }
    })
