mS3faLogLikelihood <- function(inputData,outputData,mu_z,Sigma_z,mu,lambda,psi) {
  logLikelihood <- function(data,params) {
    result <- 0
    for(i in 1:nrow(data)) {
      
      x_i <- convertToColumnVector(data[i,,drop=FALSE])
      indexes <- which(!is.na(x_i))
      result <- result + getMarginalDistributionLogPdf(x_i,params$mu,params$Sigma,indexes)
    }
    result
  }
  
  v1 <- mu + lambda %*% mu_z
  v2 <- mu_z
  v <- rbind(v1,v2)
  
  M1 <- lambda %*% Sigma_z %*% t(lambda) + psi
  M2 <- lambda %*% Sigma_z
  M3 <- t(M2)
  M4 <- Sigma_z
  M <- rbind(cbind(M1,M2),cbind(M3,M4))
  
  data <- cbind(inputData,outputData)

  params <- list(
    mu=v,
    Sigma=M
  )
  result <- logLikelihood(data,params) 
  result
}