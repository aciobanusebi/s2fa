faInitCheckArgs <- function(X_t_unsupervised,nDimZ,type) {
  checkMatrix(X_t_unsupervised)
  checkType(type)
  
  nDimX <- ncol(X_t_unsupervised)
  nTrain <- nrow(X_t_unsupervised)
  if(nDimX > nTrain) {
    stop("X_t_unsupervised: the number of columns must not be greater than the number of rows")
  }
  if(nDimZ > nTrain) {
    stop("nDimZ must not be greater than the number of rows of 'X_t_unsupervised'")
  }
}