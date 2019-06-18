#' Predict new values, given the parameters of a trained FA model
#'
#' @param p parameters of a fitted FA
#' @param X_t_test a matrix containing the input data; their output values would be predicted
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#' @param checkPositiveDefinite whether to check the covariance matrices are valid; it takes more time to execute
#'
#' @return a list containing the transposed predicted values ($values_t) and the covariance matrix associated with them ($Sigma_t)
#' @export
#'
#' @examples
#' params <- faFit(X_t_unsupervised = house[1:20,2:3,drop=FALSE],
#'                 paramsNULL_nDimZ = 2,
#'                 params=NULL,
#'                 type="fa",
#'                 lambdaRidge=0,
#'                 checkArgs=FALSE,
#'                 checkPositiveDefinite=FALSE,
#'                 epsilon=1e-10,
#'                 maxIterations=100,
#'                 stopType="parameter",
#'                 turboEmMethods=NULL)
#' result <- faPredict(p = params,
#'                     X_t_test = house[21:26,2:3,drop=FALSE],
#'                     checkArgs=TRUE,
#'                     checkPositiveDefinite=FALSE)
#' result
#'
faPredict <- function(p,X_t_test,checkArgs=TRUE,checkPositiveDefinite=FALSE) {
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }

  if(checkArgs) {
    faPredictCheckArgs(p,X_t_test,checkPositiveDefinite)
  }

  nDimX <- ncol(X_t_test)
  nTest <- nrow(X_t_test)

  posDefMatrix <- t(p$lambda_t) %*% p$lambda_t
  if(p$nDimX != 1 && (p$type %in% c("ppca","fa"))) {
    diag(posDefMatrix) <- diag(posDefMatrix) + c(p$psi_t)
  } else {
    posDefMatrix <- posDefMatrix + p$psi_t
  }

  term_t <- chol2inv(chol(posDefMatrix)) %*% t(p$lambda_t)
  sigma_t <- diag(p$nDimZ) - p$lambda_t %*% term_t

  values_t <- (X_t_test - matrix(rep(1,nTest)) %*% p$mu_t) %*% term_t

  list(
    values_t=values_t,
    Sigma_t=sigma_t
  )
}
