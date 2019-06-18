checkMatrices <- function(X_t,Z_t) {
  # X_t - checked
  # z_t - checked
  if(nrow(X_t) != nrow(Z_t)) {
    stop("Number of rows must be the same in the two matrices")
  }
}