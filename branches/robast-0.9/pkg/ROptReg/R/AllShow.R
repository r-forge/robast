setMethod("show", "RegTypeFamily", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        cat("\n### ErrorDistr:\t")
        print(object@ErrorDistr)
        cat("\n### RegDistr:\t")
        print(object@RegDistr)
        cat("\n### param:\t")
        show(object@param)
        if(length(object@props) != 0){
            cat("\n### props:\n")
            show(object@props)
        }
    })
setMethod("show", "CondNeighborhood", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("type:\t", object@type, "\n")                
        cat("radius:\t", object@radius, "\n")                
#        cat("radiusCurve:\n")
#        print(object@radiusCurve)
    })
setMethod("show", "AvCondNeighborhood", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("type:\t", object@type, "\n")                
        cat("radius:\t", object@radius, "\n")                
#        cat("radiusCurve:\n")
#        print(object@radiusCurve)
#        cat("exponent:\t", object@exponent, "\n")
    })
setMethod("show", "InfRobRegTypeModel",
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("###### center:\t")
        show(object@center)
        cat("\n###### neighborhood:\t")
        show(object@neighbor)
    })
setMethod("show", "CondContIC", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        L2Fam <- eval(object@CallL2Fam)
        cat("\n### L2-differentiable regression type family:\n", L2Fam@name, "\n")
        cat("### param:\t")
        show(L2Fam@param)
        cat("\n### neighborhood radius:\t", object@neighborRadius, "\n")
        cat("\n### clip:\t")
        show(object@clip)                
        cat("### cent:\t")
        show(object@cent)                
        cat("### stand:\n")
        show(object@stand)   
        if(!is.null(object@lowerCase)){
            cat("### lowerCase:\t")
            show(object@lowerCase)   
        }
        cat("\n### Infos:\n")
        show(object@Infos)
    })
setMethod("show", "Av1CondContIC", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        L2Fam <- eval(object@CallL2Fam)
        cat("\n### L2-differentiable regression type family:\n", L2Fam@name, "\n")
        cat("### param:\t")
        show(L2Fam@param)
        cat("\n### neighborhood radius:\t", object@neighborRadius, "\n")
        cat("\n### clip:\t")
        show(object@clip)                
        cat("### cent:\t")
        show(object@cent)                
        cat("### stand:\n")
        show(object@stand)   
        if(!is.null(object@lowerCase)){
            cat("### lowerCase:\t")
            show(object@lowerCase)   
        }
        cat("\n### Infos:\n")
        show(object@Infos)
    })
setMethod("show", "Av2CondContIC", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        L2Fam <- eval(object@CallL2Fam)
        cat("\n### L2-differentiable regression type family:\n", L2Fam@name, "\n")
        cat("### param:\t")
        show(L2Fam@param)
        cat("\n### neighborhood radius:\t", object@neighborRadius, "\n")
        cat("\n### clip:\t")
        show(object@clip)                
        cat("### cent:\t")
        show(object@cent)                
        cat("### stand:\t")
        show(object@stand)   
        if(!is.null(object@lowerCase)){
            cat("### lowerCase:\t")
            show(object@lowerCase)   
        }
        cat("\n### Infos:\n")
        show(object@Infos)
    })
setMethod("show", "CondTotalVarIC", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        L2Fam <- eval(object@CallL2Fam)
        cat("\n### L2-differentiable regression type family:\n", L2Fam@name, "\n")
        cat("### param:\t")
        show(L2Fam@param)
        cat("\n### neighborhood radius:\t", object@neighborRadius, "\n")
        cat("\n### clipUp:\t")
        show(object@clipUp)
        cat("### clipLo:\t")
        show(object@clipLo)                
        cat("### stand:\n")
        show(object@stand)   
        cat("\n### Infos:\n")
        show(object@Infos)
    })
setMethod("show", "Av1CondTotalVarIC", 
    function(object){
        cat(paste("An object of class", dQuote(class(object)), "\n"))
        cat("### name:\t", object@name, "\n")
        L2Fam <- eval(object@CallL2Fam)
        cat("\n### L2-differentiable regression type family:\n", L2Fam@name, "\n")
        cat("### param:\t")
        show(L2Fam@param)
        cat("\n### neighborhood radius:\t", object@neighborRadius, "\n")
        cat("\n### clipUp:\t")
        show(object@clipUp)
        cat("### clipLo:\t")
        show(object@clipLo)                
        cat("### stand:\n")
        show(object@stand)   
        cat("\n### Infos:\n")
        show(object@Infos)
    })
