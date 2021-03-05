mS3faFitIteration <- function(trainInput, trainOutput, params, type="fa", withCorrection=FALSE, lambdaRidge=lambdaRidge) {
  nTrain <- nrow(trainInput)

  E <- mS3faFitE(trainInput,trainOutput,params)
  E_x <- E$E_x
  E_xx <- E$E_xx
  E_z <- E$E_z
  E_zz <- E$E_zz
  E_xz <- E$E_xz

  E_xx_sum <- 0
  E_x_sum <- 0
  E_zz_sum <- 0
  E_z_sum <- 0
  E_xz_sum <- 0
  for(i in 1:nTrain) {
    E_xx_sum <- E_xx_sum + E_xx[[i]]
    E_x_sum <- E_x_sum + E_x[[i]]
    E_zz_sum <- E_zz_sum + E_zz[[i]]
    E_z_sum <- E_z_sum + E_z[[i]]
    E_xz_sum <- E_xz_sum + E_xz[[i]]
  }

  mu_z <- E_z_sum / nTrain

  Sigma_z <- E_zz_sum - E_z_sum %*% t(mu_z) - mu_z %*% t(E_z_sum) + nTrain*mu_z %*% t(mu_z)


  if(withCorrection) {
    Sigma_z <- Sigma_z / (nTrain - 1)
  } else {
    Sigma_z <- Sigma_z / nTrain
  }

  mu_x <- E_x_sum/nTrain

  lambdaRight <- E_zz_sum + lambdaRidge * diag(ncol(trainOutput))
  print(nTrain * mu_z %*% t(mu_z) - lambdaRight)
  lambda <- (nTrain * mu_x %*% t(mu_z) - E_xz_sum) %*% solve(nTrain * mu_z %*% t(mu_z) - lambdaRight)

  mu <- mu_x - lambda %*% mu_z

  firstTerm <- E_xx_sum - E_x_sum %*% t(mu) - mu %*% t(E_x_sum) + nTrain*mu %*% t(mu)
  secondTerm <- E_xz_sum %*% t(lambda) - mu %*% t(E_z_sum) %*% t(lambda)
  thirdTerm <- t(secondTerm)
  fourthTerm <- lambda %*% E_zz_sum %*% t(lambda)

  psi <- firstTerm - secondTerm - thirdTerm + fourthTerm

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
