s3faFitIteration <- function(X_t_supervised, Z_t_supervised, X_t_unsupervised,
                    params, type="fa",lambdaRidge=0,
                    checkArgs=TRUE,
                    checkPositiveDefinite=FALSE) {
  
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  
  if(!is.null(params$type) && params$type != type) {
    stop("params$type != type")
  }
  eResult <- s3faFitE(X_t_supervised, Z_t_supervised, X_t_unsupervised, params,
                      checkArgs = checkArgs,
                      checkPositiveDefinite)
  mResult <- s3faFitM(eResult$X_t, 
                      eResult$Z_t, 
                      eResult$ZZ_t,
                      eResult$ZX_t,
                      eResult$XX_t,
                      type = type, 
                      lambdaRidge=lambdaRidge,
                      checkArgs = checkArgs)
  mResult
}
