s2faPlotCheckArgs <- function(X_t,Z_t,p,
                              add,
                              addPoints,
                              addLr,
                              checkPositiveDefinite) {
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  
  checkMatrix(X_t)
  checkMatrix(Z_t)
  checkParams(p,checkPositiveDefinite)
  checkMatrices(X_t,Z_t)
  checkParamsMatrixX(p,X_t)
  checkParamsMatrixZ(p,Z_t)
  
  if(!is.logical(add)) {
    stop("add must be TRUE or FALSE")
  }
  if(!is.logical(addPoints)) {
    stop("addPoints must be TRUE or FALSE")
  }
  if(!is.logical(addLr)) {
    stop("addLr must be TRUE or FALSE")
  }
}
