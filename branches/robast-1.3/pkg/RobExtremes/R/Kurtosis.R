###################################################################################
#kurtosis  --- code due to G. Jay Kerns, gkerns@ysu.edu
###################################################################################


setMethod("kurtosis", signature(x = "Pareto"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else{
         a <- shape(x)
         if(a<=4) return(NA)
         else{
         ret.v <- ( 6*(a^3+a^2-6*a-2)/a/(a-3)/(a-4) )
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }
    }
})
### source https://mathworld.wolfram.com/ParetoDistribution.html

setMethod("kurtosis", signature(x = "Gumbel"),
    function(x, ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else{
         return(12/5)
# https://mathworld.wolfram.com/GumbelDistribution.html         
    }
})

setMethod("kurtosis", signature(x = "GPareto"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else{
         k <- shape(x)
         if(k>=1/4) return(NA)
         else{
         ret.v<- ( 3*(3+k+2*k^2)*(1-2*k)/(1-4*k)/(1-3*k)-3)
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }}
})
### source Maple ...

setMethod("kurtosis", signature(x = "GEV"),
    function(x, propagate.names=getdistrExOption("propagate.names.functionals"), ...){
    dots <- match.call(call = sys.call(sys.parent(1)), 
                       expand.dots = FALSE)$"..."
    fun <- NULL; cond <- NULL; low <- NULL; upp <- NULL
    if(hasArg(low)) low <- dots$low
    if(hasArg(upp)) upp <- dots$upp
    if(hasArg(fun)||hasArg(cond)||!is.null(low)||!is.null(upp)) 
        return(kurtosis(as(x,"AbscontDistribution"),...))
    else{
         xi <- shape(x)
         if(xi>=1/4) return(NA)
         if(xi==0) return(12/5)
         else{
         ret.v <- ((gamma(1-4*xi)- 4*gamma(1-xi)*gamma(1-3*xi)+6*gamma(1-2*xi)*gamma(1-xi)^2 - 3*gamma(1-xi)^4)/(gamma(1-2*xi)-gamma(1-xi)^2)^(2))
         if(!propagate.names){names(ret.v) <- NULL}
         return(ret.v)
    }}
})
### source https://en.wikipedia.org/wiki/Generalized_extreme_value_distribution
###        https://en.wikipedia.org/wiki/Gumbel_distribution
###        https://en.wikipedia.org/wiki/Riemann_zeta_function 
