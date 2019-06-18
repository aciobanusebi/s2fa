s3faFitMCheckArgs <- function(X_t, Z_t, 
                              ZZ_t,
                              ZX_t,
                              XX_t,
                              type, 
                              lambdaRidge) {
  checkMatrix(X_t)
  checkMatrix(Z_t)
  checkMatrices(X_t,Z_t)
  checkMatrix(ZZ_t)
  checkMatrix(ZX_t)
  checkMatrix(XX_t)
  checkType(type)
  checkLambdaRidge(lambdaRidge)
  
  if(!isSymmetric(ZZ_t)) {
    stop("ZZ_t must be symmetric")
  }
  if(!isSymmetric(XX_t)) {
    stop("XX_t must be symmetric")
  }
  if(ncol(X_t) != ncol(XX_t) ||
     ncol(Z_t) != ncol(ZZ_t) ||
     ncol(X_t) != ncol(ZX_t) ||
     ncol(Z_t) != nrow(ZX_t)) {
    stop("matrix dimensions error")
  }
}