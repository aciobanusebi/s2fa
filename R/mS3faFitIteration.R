mS3faFitIteration <- function(trainInput, trainOutput, params, type="fa", withCorrection=FALSE, lambdaRidge=lambdaRidge) {
  nTrain <- nrow(trainInput)
  
  E <- mS3faFitE(trainInput,trainOutput,params)
  E_x <- E$E_x
  E_xx <- E$E_xx
  E_z <- E$E_z
  E_zz <- E$E_zz
  E_xz <- E$E_xz
  
  mu_z <- 0
  for(i in 1:nTrain) {
    mu_z <- mu_z + E_z[[i]]
  }
  mu_z <- mu_z / nTrain
  
  Sigma_z <- 0
  for(i in 1:nTrain) {
    Sigma_z <- Sigma_z + E_zz[[i]] - E_z[[i]] %*% t(mu_z) - mu_z %*% t(E_z[[i]]) + mu_z %*% t(mu_z)
  }
  if(withCorrection) {
    Sigma_z <- Sigma_z / (nTrain - 1)
  } else {
    Sigma_z <- Sigma_z / nTrain
  }
  
  mu_x <- 0
  for(i in 1:nTrain) {
    mu_x <- mu_x + E_x[[i]]
  }
  mu_x <- mu_x/nTrain
  
  lambdaLeft <- 0
  lambdaRight <- 0
  for(i in 1:nTrain) {
    lambdaLeft <- lambdaLeft + E_xz[[i]]
    lambdaRight <- lambdaRight + E_zz[[i]]
  }
  lambdaRight <- lambdaRight + lambdaRidge * diag(ncol(trainOutput))
  lambda <- (nTrain * mu_x %*% t(mu_z) - lambdaLeft) %*% solve(nTrain * mu_z %*% t(mu_z) - lambdaRight)
  
  mu <- mu_x - lambda %*% mu_z
  
  psi <- 0
  for(i in 1:nTrain) {
    firstTerm <- E_xx[[i]] - E_x[[i]] %*% t(mu) - mu %*% t(E_x[[i]]) + mu %*% t(mu)
    secondTerm <- E_xz[[i]] %*% t(lambda) - mu %*% t(E_z[[i]]) %*% t(lambda)
    thirdTerm <- t(secondTerm)
    fourthTerm <- lambda %*% E_zz[[i]] %*% t(lambda)
    
    psi <- psi + firstTerm - secondTerm - thirdTerm + fourthTerm
  }
  psi <- psi / nTrain
  
  if(type == "fa") {
    if(length(psi) != 1) {
      psi <- diag(diag(psi))
    }
  } else if(type == "ppca") {
    nDimTrain <- ncol(trainInput)
    nu <- sum(diag(psi)) / nDimTrain
    psi <- diag(nDimTrain) * nu
  }
  
  list(
    mu_z=mu_z,
    Sigma_z=Sigma_z,
    mu=mu,
    lambda=lambda,
    psi=psi
  )
}
