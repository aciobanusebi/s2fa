faFitM <- function(mu_t, nTrain,
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
    faFitMCheckArgs(mu_t, 
                      ZZ_t,
                      ZX_t,
                      XX_t,
                      type, 
                      lambdaRidge)
  }
  
  # nTrain <- nrow(mu_t)
  nDimX <- ncol(mu_t)
  nDimZ <- ncol(ZZ_t)
  
  # mu_z_t <- t(colMeans(Z_t)) # row vector
  # Sigma_z_t <- ZZ_t - nTrain * t(mu_z_t) %*% mu_z_t
  # Sigma_z_t <- Sigma_z_t/nTrain
  
  # mu_x_t <- t(colMeans(X_t)) # row vector
  mu_x_t <- mu_t
  aux <- try(solve(- ZZ_t - lambdaRidge * diag(nDimZ)))
  if(class(aux) == "try-error") {
    stop("Try setting 'lambdaRidge' argument to a number != 0")
  }
  lambda_t <- aux %*%
    (- ZX_t)
  # mu_t <- mu_x_t
  
  term1 <- nTrain * t(mu_x_t) %*% mu_t
  term2 <- t(lambda_t) %*% ZX_t 
  psi_t <- XX_t - term1 - t(term1) + nTrain * t(mu_t) %*% mu_t - 
    t(term2) -
    term2 + 
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
    mu_t=mu_t,
    lambda_t=lambda_t,
    psi_t = psi_t
  )
    
}