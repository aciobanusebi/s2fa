checkMatricesXX <- function(X_t1,X_t2) {
  # X_t1 - checked
  # X_t2 - checked
  if(ncol(X_t1) != ncol(X_t2)) {
    stop("Input matrices must have the same number of columns")
  }
}