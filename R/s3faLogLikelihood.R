s3faLogLikelihood <- function(X_t_supervised,Z_t_supervised,
                              X_t_unsupervised,params,
                              checkArgs=TRUE,
                              checkPositiveDefinite=FALSE) {

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

  lambda_psi_t <- t(params$lambda_t) %*% params$Sigma_z_t %*% params$lambda_t
  params$type <- tolower(params$type)
  if(params$type %in% c("ppca","fa")) {
    diag(lambda_psi_t) <- diag(lambda_psi_t) + params$psi_t
  } else {
    lambda_psi_t <- lambda_psi_t + params$psi_t
  }
  mu_x_t <- params$mu_t + params$mu_z_t %*% params$lambda_t

  result <- sum(myLogNormal_t(X_t_unsupervised,mu_x_t,lambda_psi_t))

  for(i in 1:nrow(X_t_supervised)) {
    x_i_t <- X_t_supervised[i,,drop=FALSE]
    z_i_t <- Z_t_supervised[i,,drop=FALSE]
    result <- result + myLogNormal_t(z_i_t,params$mu_z_t,params$Sigma_z_t)
    auxSigma <- params$psi_t
    if(params$type == "ppca") {
      auxSigma <- diag(params$nDimX) * params$psi_t
    } else if(params$type == "fa") {
      if(params$nDimX == 1) {
        auxSigma <- matrix(params$psi_t)
      } else {
        auxSigma <- diag(params$psi_t)
      }
    }
    result <- result + myLogNormal_t(x_i_t,params$mu_t + z_i_t %*% params$lambda_t,
                                     auxSigma)
  }
  as.numeric(result)
}
