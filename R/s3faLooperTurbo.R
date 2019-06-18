s3faLooperTurbo <- function(params,objfn,fixptfn,
                       localHasConvergedParameter,
                       localHasConvergedLogLike,
                       maxIterations,
                       stopType,
                       methods=c("em", "squarem", "pem")) {

  if(stopType == "parameter") {
    controlRun <- list(
      maxiter=maxIterations,
      convfn.user=localHasConvergedParameter,
      convtype="parameter"
    )
  } else {
    controlRun <- list(
      maxiter=maxIterations,
      convfn.user=localHasConvergedLogLike,
      convtype="objfn"
    )
  }

  res <- turboEM::turboem(par=getParamsAsVector(params), fixptfn=fixptfn, objfn=objfn,
                 method=methods,
                 control.run=controlRun)
  cat("The params from the 1st method were returned. Summary info below:\n")
  print(res)
  newParams <- getParamsFromVector(res$pars[1,],params$type,params$nDimX,params$nDimZ,params)
  # stopType in "parameter", "objfn"
  newParams
}
