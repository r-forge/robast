

setMethod("cutoff.quantile", "cutoff", function(object)object@cutoff.quantile)
setReplaceMethod("cutoff.quantile", "cutoff", function(object, value){
                     object@cutoff.quantile<-value; object})

setMethod("fct", "cutoff", function(object)object@fct)
setMethod("name", "cutoff", function(object)object@name)



cutoff <- function(name = "empirical",
                   body.fct0,
                   cutoff.quantile  = 0.95,
                   norm = NormType(), QF, nsim = 100000){
   mc <- match.call()
   if (missing(body.fct0)) body.fct0 <- quote(quantile(slot(norm,"fct")(data), cutoff.quantile))
   arglist <- list()
   arglist$norm <- if (!is.null(mc$norm)) mc$norm else NormType()
   QFsub <- if(is.null(mc$QF))
                quote({QF <- if(is(norm,"QFNorm"))
                                QuadForm(norm) else diag(nrow(data))})
            else quote({})
   arglist$cutoff.quantile <- if (!is.null(mc$cutoff.quantile))
                                   mc$cutoff.quantile else 0.95
   arglist$nsim <- if (!is.null(mc$nsim)) mc$nsim else 100000
   arglist$QFsub0 <- substitute(QFsub)
   arglist$body.fct1 <- body.fct0
   fct0 <- function(data){}
   body(fct0) <- #eval(
                 substitute({
            QFsub0
            body.fct1
            }, arglist)
           #)
   new("cutoff", fct = fct0, name = name, cutoff.quantile = cutoff.quantile)
}

cutoff.sememp <- function(cutoff.quantile  = 0.95){cutoff(name = "semi-empirical",
                   body.fct0 = substitute({n.05 <- chol(QF)
#                                  print(QF)
                                  N0 <- matrix(rnorm(nsim*nrow(QF)),ncol=ncol(QF))
                                  N0 <- N0 %*% n.05
                                  quantile((rowSums(N0^2))^.5,cutoff.quantile0)
                                  }, list(cutoff.quantile0  = cutoff.quantile))
                                  )}

cutoff.chisq <- function(cutoff.quantile  = 0.95){cutoff(name = "chisq",
                   body.fct0 = substitute({dim = nrow(as.matrix(data))
                                  qchisq(df = dim, cutoff.quantile0)^.5
                                  }, list(cutoff.quantile0  = cutoff.quantile)))}

cutoff.quant <- function(qfct){
                   if(missing(qfct)) qfct <- NULL
                   cutoff(name = "quantile",
                   body.fct0 = substitute({
                                  if(is.null(qfctA)){
                                     if(exists("..ICloc")){
                                        L2m <- eval(CallL2Fam(get("..ICloc")))
                                        qfct0 <- q(L2m)
                                     }else{
                                        qfct0 <- qnorm
                                     }
                                  }else{
                                     qfct0 <- qfctA
                                  }
                                  q0 <- qfct0(cutoff.quantile)
                                  if(exists("..trf")){
                                     q0 <- get("..trf")(q0)
                                  }
                                  return(q0)
                                }, list(qfctA=qfct)),
                   cutoff.quantile  = 0.95)}
