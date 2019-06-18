faFitE <- function(X_t_unsupervised, params,
                     checkArgs = TRUE,
                     checkPositiveDefinite) {
  
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  
  if(checkArgs) {
    faFitECheckArgs(X_t_unsupervised,
                      params,
                      checkPositiveDefinite)
  }
  
  X_t <- X_t_unsupervised
  
  aux <- faPredict_(params,X_t_unsupervised,checkArgs,checkPositiveDefinite)
  
  rm(X_t_unsupervised)
  
  Z_t <- aux$E_z
  
  ZZ_t <- aux$E_zz
  
  rm(aux)
  
  ZX_t <- t(Z_t) %*% X_t
  
  XX_t <- t(X_t) %*% X_t
  
  list(
    ZZ_t=ZZ_t,
    ZX_t=ZX_t,
    XX_t=XX_t
  )
}
