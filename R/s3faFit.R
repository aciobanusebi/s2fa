#' Fit S3FA to data via EM/S3FA
#'
#' @param X_t_supervised train input data (which has output) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param Z_t_supervised train output data (for X_t_supervised) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param X_t_unsupervised train input data (which has no output) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param params initialization parameters; can be NULL if no parameters are to be provided; check fitFaInit function if you want to set this
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#' @param lambdaRidge L2 regularization term; must be a number
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#' @param checkPositiveDefinite whether to check the covariance matrices are valid; it takes more time to execute
#' @param epsilon tolerance parameter regarding iteration stop; the stop criterion is something like this: if |old-new|/|old| < epsilon then STOP, where old, new are loglikelihoods (|a| is module) or parameters (|a| is L2 norm squared); this last part is controlled via "stopType" parameter
#' @param maxIterations maximum number of iterations
#' @param stopType "parameter" or "objfn"
#' @param turboEmMethods if it is NULL, then a plot of log-likelihoods is provided; if it is not NULL, it must be a vector of methods accepted in the 'turboEM' package, e.g. c("em","squarem","pem"), and, in this case, the parameters corresponding to the first method in turboEmMethods are returned; additional information may be printed
#'
#' @return final parameters learnt by EM/S3FA, i.e. a list containing nDimX, nDimZ, type, mu_z_t, Sigma_z_t, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#' @export
#'
#' @examples
#' params0 <- s3faInit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                     Z_t_supervised = house[1:10,1,drop=FALSE],
#'                     X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                     type = "fa",
#'                     checkArgs = TRUE)
#' params <- s3faFit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                   Z_t_supervised = house[1:10,1,drop=FALSE],
#'                   X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                   params=params0,
#'                   type="fa",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE,
#'                   checkPositiveDefinite=FALSE,
#'                   epsilon=1e-10,
#'                   maxIterations=100,
#'                   stopType="parameter",
#'                   turboEmMethods=c("em","pem"))
#' params
#'
#' params <- s3faFit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                   Z_t_supervised = house[1:10,1,drop=FALSE],
#'                   X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                   params=NULL,
#'                   type="un",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE,
#'                   checkPositiveDefinite=FALSE,
#'                   epsilon=1e-10,
#'                   maxIterations=100,
#'                   stopType="objfn",
#'                   turboEmMethods=NULL)
#' params
#'
s3faFit <- function(X_t_supervised, Z_t_supervised, X_t_unsupervised,
                    params=NULL, type="fa",lambdaRidge=0,
                    checkArgs=TRUE,
                    checkPositiveDefinite=FALSE,
                    epsilon=1e-10,
                    maxIterations=100,
                    stopType="parameter",
                    turboEmMethods=NULL) {
  # example for turboEmMethods: c("em", "squarem", "pem")
  # stopType in {"parameter","objfn"}

  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }

  if(is.null(params)) {
    params <- s3faInit(X_t_supervised,
                       Z_t_supervised,
                       X_t_unsupervised,
                       type,checkArgs)
  }

  if(!(is.numeric(epsilon) && length(epsilon) == 1)) {
    stop("epsilon must be a number")
  }
  if(!(floor(maxIterations) ==  maxIterations && length(maxIterations) == 1)) {
    stop("maxIterations must be an integer")
  }
  if(!(stopType %in% c("parameter","objfn"))) {
    stop("stopType must be 'parameter' or 'objfn'")
  }

  fixptfn <- function(params) {
    if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
      params <- getParamsFromVector(params,type,ncol(X_t_supervised),ncol(Z_t_supervised))
    }
    params <- s3faFitIteration(X_t_supervised, Z_t_supervised, X_t_unsupervised,
                     params, type,lambdaRidge,
                     checkArgs,
                     checkPositiveDefinite)
    if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
      return(getParamsAsVector(params))
    }
    params
  }
  objfn <- function(params) {
    if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
      params <- getParamsFromVector(params,type,ncol(X_t_supervised),ncol(Z_t_supervised))
    }
    -s3faLogLikelihood(X_t_supervised,Z_t_supervised,
                      X_t_unsupervised,params,
                      checkArgs,
                      checkPositiveDefinite)
  }

  MY_EPSILON_S2FA <<- epsilon # hack to work with turboEM, otherwise error
  localHasConvergedLogLike <- function(new,old) {
    s2fa::hasConvergedLogLike(new,old,MY_EPSILON_S2FA)
  }

  localHasConvergedParameter <- function(new,old) {
    s2fa::hasConvergedParameter(new,old,MY_EPSILON_S2FA)
  }

  if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
    result <- s3faLooperTurbo(params,objfn,fixptfn,
                                localHasConvergedParameter,
                                localHasConvergedLogLike,
                                maxIterations,
                                stopType,
                                methods=turboEmMethods)
  } else {
    result <- s3faLooper(params,objfn,fixptfn,
               localHasConvergedParameter,
               localHasConvergedLogLike,
               maxIterations,
               stopType)
  }
  rm(MY_EPSILON_S2FA,envir=globalenv())
  result

}
