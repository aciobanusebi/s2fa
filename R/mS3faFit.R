#' Fit MS3FA to data via EM/MS3FA
#'
#' @param trainInput train input data; can contain NAs; error if a full row (input+output) is NA
#' @param trainOutput train output data; can contain NAs; error if a full row (input+output) is NA
#' @param params initialization parameters; can be NULL if no parameters are to be provided; check fitFaInit function if you want to set this
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#' @param lambdaRidge L2 regularization term; must be a number
#' @param epsilon tolerance parameter regarding iteration stop; the stop criterion is something like this: if |old-new|/|old| < epsilon then STOP, where old, new are loglikelihoods (|a| is module) or parameters (|a| is L2 norm squared); this last part is controlled via "stopType" parameter
#' @param maxIterations maximum number of iterations
#' @param stopType "parameter" or "objfn"
#' @param turboEmMethods if it is NULL, then a plot of log-likelihoods is provided; if it is not NULL, it must be a vector of methods accepted in the 'turboEM' package, e.g. c("em","squarem","pem"), and, in this case, the parameters corresponding to the first method in turboEmMethods are returned; additional information may be printed
#'
#' @return final parameters learnt by EM/MS3FA, i.e. a list containing nDimX, nDimZ, type, mu_z_t, Sigma_z_t, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#'
#' @export
#' @examples
#' houseCopy <- house
#' houseCopy[1:10,2:3] <- NA
#' houseCopy[11:20,1] <- NA
#' params0 <- mS3faInit(trainInput=houseCopy[,2:3,drop=FALSE],
#'                      trainOutput=houseCopy[,1,drop=FALSE],
#'                      type="fa")
#' params <- mS3faFit(trainInput=houseCopy[,2:3,drop=FALSE],
#'                    trainOutput=houseCopy[,1,drop=FALSE],
#'                    params=params0,
#'                    type="fa",
#'                    lambdaRidge=0,
#'                    epsilon=1e-10,
#'                    maxIterations=100,
#'                    stopType="parameter",
#'                    turboEmMethods=c("em","pem"))
#' params <- mS3faFit(trainInput=houseCopy[,2:3,drop=FALSE],
#'                    trainOutput=houseCopy[,1,drop=FALSE],
#'                    params=NULL,
#'                    type="ppca",
#'                    lambdaRidge=0,
#'                    epsilon=1e-10,
#'                    maxIterations=100,
#'                    stopType="objfn",
#'                    turboEmMethods=NULL)
mS3faFit <- function(trainInput,
                     trainOutput,
                     params,
                     type="fa",
                     lambdaRidge=lambdaRidge,
                      epsilon=1e-10,
                      maxIterations=100,
                      stopType="parameter",
                      turboEmMethods=NULL) {
  # example for turboEmMethods: c("em", "squarem", "pem")
  # stopType in {"parameter","objfn"}

  if(is.null(params)) {
    params <- mS3faInit(trainInput,trainOutput,type)
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
      params <- getParamsFromVector(params,type,ncol(trainInput),ncol(trainOutput))
    }
    params <- mS3faFitIteration(trainInput, trainOutput, params,
                                type=type,
                                withCorrection=FALSE,
                                lambdaRidge=lambdaRidge)
    if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
      return(getParamsAsVector(params))
    }
    params
  }
  objfn <- function(params) {
    if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
      params <- getParamsFromVector(params,type,ncol(trainInput),ncol(trainOutput))
    }
    -mS3faLogLikelihood(trainInput,trainOutput,params$mu_z,params$Sigma_z,params$mu,params$lambda,params$psi)
  }

  MY_EPSILON_S2FA <<- epsilon # hack to work with turboEM, otherwise error
  localHasConvergedLogLike <- function(new,old) {
    s2fa::hasConvergedLogLike(new,old,MY_EPSILON_S2FA)
  }

  localHasConvergedParameter <- function(new,old) {
    s2fa::hasConvergedParameter(new,old,MY_EPSILON_S2FA)
  }

  if(!(any(is.na(turboEmMethods)) || is.null(turboEmMethods))) {
    result <- mS3faLooperTurbo(params,objfn,fixptfn,
                            localHasConvergedParameter,
                            localHasConvergedLogLike,
                            maxIterations,
                            stopType,
                            methods=turboEmMethods)
  } else {
    result <- mS3faLooper(params,objfn,fixptfn,
                       localHasConvergedParameter,
                       localHasConvergedLogLike,
                       maxIterations,
                       stopType)
  }
  rm(MY_EPSILON_S2FA,envir=globalenv())
  result

}
