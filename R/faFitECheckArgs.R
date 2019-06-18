faFitECheckArgs <- function(X_t_unsupervised,
                          params,
                          checkPositiveDefinite) {

  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }

  checkMatrix(X_t_unsupervised)
  checkParams_fa(params,checkPositiveDefinite)
  checkParamsMatrixX(params,X_t_unsupervised)
  checkType(params$type)

  nDimX <- ncol(X_t_unsupervised)
  nDimZ <- params$nDimZ
  nTrain <- nrow(X_t_unsupervised)
  if(nDimX > nTrain) {
    stop("X_t_unsupervised: the number of columns must not be greater than the number of rows")
  }
  if(nDimZ > nTrain) {
    stop("p$nDimZ must not be greater than the number of rows of 'X_t_unsupervised'")
  }
}
