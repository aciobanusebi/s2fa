faPredictCheckArgs <- function(p,X_t_test,
                               checkPositiveDefinite) {
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  checkMatrix(X_t_test)
  checkParams_fa(p,checkPositiveDefinite)
  checkParamsMatrixX(p,X_t_test)
}