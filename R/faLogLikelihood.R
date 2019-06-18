faLogLikelihood <- function(X_t_unsupervised,params,
                              checkArgs=TRUE,
                              checkPositiveDefinite=FALSE) {
  
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
  lambda_psi_t <- t(params$lambda_t) %*%  params$lambda_t
  params$type <- tolower(params$type)
  if(params$type %in% c("ppca","fa")) {
    diag(lambda_psi_t) <- diag(lambda_psi_t) + params$psi_t
  } else {
    lambda_psi_t <- lambda_psi_t + params$psi_t
  }
  mu_x_t <- params$mu_t
  
  result <- sum(myLogNormal_t(X_t_unsupervised,mu_x_t,lambda_psi_t))

  as.numeric(result)
}
