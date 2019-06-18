faFitMCheckArgs <- function(mu_t, 
                              ZZ_t,
                              ZX_t,
                              XX_t,
                              type, 
                              lambdaRidge) {
  checkMatrix(mu_t)
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
  if(ncol(mu_t) != ncol(XX_t) ||
     ncol(mu_t) != ncol(ZX_t)) {
    stop("matrix dimensions error")
  }
}