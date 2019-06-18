faFitIteration <- function(X_t_unsupervised,
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
  eResult <- faFitE(X_t_unsupervised, params,
                      checkArgs = checkArgs,
                      checkPositiveDefinite)
  mResult <- faFitM(params$mu_t,  
                    nrow(X_t_unsupervised),
                      eResult$ZZ_t,
                      eResult$ZX_t,
                      eResult$XX_t,
                      type = type, 
                      lambdaRidge=lambdaRidge,
                      checkArgs = checkArgs)
  mResult
}
