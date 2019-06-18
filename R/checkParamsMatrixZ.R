checkParamsMatrixZ <- function(p,Z_t) {
  # p - checked
  # Z_t - checked
  nDimZ <- ncol(Z_t)
  if(nDimZ != p$nDimZ) {
    stop("problem with number of dimensions in p")
  }
}