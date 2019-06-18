s3faFitE <- function(X_t_supervised, Z_t_supervised, X_t_unsupervised, params,
                     checkArgs = TRUE,
                     checkPositiveDefinite) {
  
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(!is.logical(checkPositiveDefinite)) {
    stop("checkPositiveDefinite must be TRUE/FALSE")
  }
  
  if(checkArgs) {
    s3faFitECheckArgs(X_t_supervised, Z_t_supervised, X_t_unsupervised,
                      params,
                      checkPositiveDefinite)
  }
  
  X_t <- rbind(X_t_supervised,
               X_t_unsupervised)
  
  aux <- s3faPredict_(params,X_t_unsupervised,checkArgs,checkPositiveDefinite)
  
  rm(X_t_supervised)
  rm(X_t_unsupervised)
  
  Z_t <- rbind(Z_t_supervised,
               aux$E_z)
  
  ZZ_t <- t(Z_t_supervised) %*% Z_t_supervised + aux$E_zz
  
  rm(aux)
  rm(Z_t_supervised)
  
  ZX_t <- t(Z_t) %*% X_t
  
  XX_t <- t(X_t) %*% X_t
  
  list(
    X_t=X_t, 
    Z_t=Z_t, 
    ZZ_t=ZZ_t,
    ZX_t=ZX_t,
    XX_t=XX_t
  )
}
