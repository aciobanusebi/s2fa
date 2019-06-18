s3faFitM <- function(X_t, Z_t, 
                     ZZ_t,
                     ZX_t,
                     XX_t,
                    type = "fa", 
                    lambdaRidge=0,
                    checkArgs=TRUE) {
  
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  
  if(checkArgs) {
    s3faFitMCheckArgs(X_t, Z_t, 
                    ZZ_t,
                    ZX_t,
                    XX_t,
                    type, 
                    lambdaRidge)
  }
  
  nTrain <- nrow(X_t)
  nDimX <- ncol(X_t)
  nDimZ <- ncol(Z_t)
  
  mu_z_t <- t(colMeans(Z_t)) # row vector
  Sigma_z_t <- ZZ_t - nTrain * t(mu_z_t) %*% mu_z_t
  Sigma_z_t <- Sigma_z_t/nTrain
  
  mu_x_t <- t(colMeans(X_t)) # row vector
  aux <- try(solve(nTrain * crossprod(mu_z_t) - ZZ_t - lambdaRidge * diag(nDimZ)))
  if(class(aux) == "try-error") {
    stop("Try setting 'lambdaRidge' argument to a number != 0")
  }
  lambda_t <- aux %*%
    (nTrain * t(mu_z_t) %*% mu_x_t - ZX_t)
  mu_t <- mu_x_t - mu_z_t %*% lambda_t
  
  term1 <- nTrain * t(mu_x_t) %*% mu_t
  term2 <- t(lambda_t) %*% ZX_t 
  term3 <- nTrain * t(mu_t) %*% mu_z_t %*% lambda_t
  psi_t <- XX_t - term1 - t(term1) + nTrain * t(mu_t) %*% mu_t - 
    t(term2) + term3 -
    term2 + t(term3) + 
    t(lambda_t) %*% ZZ_t %*% lambda_t
  psi_t <- psi_t / nTrain

  if(type == "fa") {
    psi_t <- diag(psi_t)
  } else if(type == "ppca") {
    psi_t <- sum(diag(psi_t)) / nDimX
  }
  
  list(
    nDimX=nDimX,
    nDimZ=nDimZ,
    type=type,
    mu_z_t = mu_z_t,
    Sigma_z_t = Sigma_z_t,
    mu_t=mu_t,
    lambda_t=lambda_t,
    psi_t = psi_t
  )
  
}