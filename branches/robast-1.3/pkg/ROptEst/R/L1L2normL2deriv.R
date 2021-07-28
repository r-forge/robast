getL2normL2deriv <-
        function(aFinfo, cent, ...){sqrt(aFinfo+cent^2)}

setMethod("getL1normL2deriv", signature(L2deriv = "UnivariateDistribution"),
    function(L2deriv, cent, ...){
        return(-2*m1df(L2deriv, cent) +cent*(2*p(L2deriv)(cent)-1))
    })

setMethod("getL1normL2deriv", signature(L2deriv = "RealRandVariable"),
    function(L2deriv, cent, stand, Distr, normtype, ...){

        dotsI <- .filterEargsWEargList(list(...))
        if(is.null(dotsI$useApply)) dotsI$useApply <- FALSE

        integrandG <- function(x){
            X <- evalRandVar(L2deriv, as.matrix(x))[,,1] - cent
            Y <- apply(X, 2, "%*%", t(stand))
            res <- fct(normtype)(Y)
            return((res > 0)*res)
        }

        retval <- do.call(E, c(list(object = Distr, fun = integrandG),dotsI))
        return(retval)
    })
