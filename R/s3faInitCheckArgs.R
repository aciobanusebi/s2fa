s3faInitCheckArgs <- function(X_t_supervised, Z_t_supervised, X_t_unsupervised,type) {
  checkMatrix(X_t_supervised)
  checkMatrix(Z_t_supervised)
  checkMatrix(X_t_unsupervised)
  checkMatrices(X_t_supervised,Z_t_supervised)
  checkMatricesXX(X_t_supervised,X_t_unsupervised)
  checkType(type)
  
  nDimX <- ncol(X_t_supervised)
  nDimZ <- ncol(Z_t_supervised)
  nTrain <- nrow(X_t_supervised) + nrow(X_t_unsupervised)
  if(nDimX > nTrain) {
    stop("X_t_supervised + X_t_unsupervised: the number of columns must not be greater than the number of rows")
  }
  if(nDimZ > nTrain) {
    stop("The number of columns of 'Z_t_supervised' must not be greater than the number of rows of 'X_t_supervised + X_t_unsupervised'")
  }
}