checkParamsMatrixX <- function(p,X_t) {
  # p - checked
  # X_t - checked
  nDimX <- ncol(X_t)
  if(nDimX != p$nDimX) {
    stop("problem with number of dimensions in p")
  }
}