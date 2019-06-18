s3faLooper <- function(params,objfn,fixptfn,
                       localHasConvergedParameter,
                       localHasConvergedLogLike,
                       maxIterations,
                       stopType) {
  # stopType in "parameter", "objfn"
  if(stopType == "parameter") {
    oldParams <- getParamsAsVector(params)
  } else {
    # objfn
    oldLogLike <- -objfn(params)
    cat("Loglikelihood: ",oldLogLike,"\n")
    logLikes <- c(oldLogLike)
  }

  for(i in 1:maxIterations) {
    cat('iteration ',i,'\n')
    params <- fixptfn(params)
    if(stopType == "parameter") {
      newParams <- getParamsAsVector(params)
      stop <- localHasConvergedParameter(newParams,oldParams)
      if(stop) {
        cat("iteration ",i,"\n")
        return(params)
      }
    }
    else {
      # objfn
      newLogLike <- -objfn(params)
      logLikes <- c(logLikes,newLogLike)
      stop <- localHasConvergedLogLike(-newLogLike,-oldLogLike)
      if(stop) {
        cat("Loglikelihood: ",newLogLike,"\n")
        cat("iteration ",i,"\n")
        plot(0:i,logLikes,type="l",xlab="iteration",ylab="Log-likelihood")
        return(params)
      }
    }
    if(stopType == "parameter") {
      oldParams <- newParams
    } else {
      # objfn
      oldLogLike <- newLogLike
      cat("Loglikelihood: ",oldLogLike,"\n")
    }
  }
  cat("STOP:maxIterations attained")
  if(stopType == "objfn") {
    plot(0:maxIterations,logLikes,type="l",xlab="iteration",ylab="Log-likelihood")
  }
  params
}
