mS3faFitE <- function(trainInput,trainOutput,params) {
  nDimX <- ncol(trainInput)
  nDimZ <- ncol(trainOutput)
  xIndexes <- 1:nDimX
  zIndexes <- nDimX + (1:nDimZ)
  
  nTrain <- nrow(trainInput)
  
  v1 <- params$mu + params$lambda %*% params$mu_z
  v2 <- params$mu_z
  v <- rbind(v1,v2)
  
  M1 <- params$lambda %*% params$Sigma_z %*% t(params$lambda) + params$psi
  M2 <- params$lambda %*% params$Sigma_z
  M3 <- t(M2)
  M4 <- params$Sigma_z
  M <- rbind(cbind(M1,M2),cbind(M3,M4))
  
  data <- cbind(trainInput,trainOutput)
  
  E_z <- list()
  E_zz <- list()
  E_x <- list()
  E_xx <- list()
  E_xz <- list()
  for(i in 1:nTrain) {
    s_i <- convertToColumnVector(data[i,,drop=FALSE])
    E_s_i <- s_i
    isNaIndexes <- which(is.na(s_i))
    
    if(length(isNaIndexes)) {
      distr <- getConditionalDistribution(s_i,v,M,isNaIndexes)
      E_s_i[isNaIndexes] <- distr$mu
      E_ss_i <- E_s_i %*% t(E_s_i)
      E_ss_i[isNaIndexes,isNaIndexes] <- E_ss_i[isNaIndexes,isNaIndexes] + distr$Sigma
    } else {
      E_ss_i <- s_i %*% t(s_i)
    }
    E_x_i <- E_s_i[xIndexes,,drop=FALSE]
    E_z_i <- E_s_i[zIndexes,,drop=FALSE]
    E_xx_i <- E_ss_i[xIndexes,xIndexes,drop=FALSE]
    E_zz_i <- E_ss_i[zIndexes,zIndexes,drop=FALSE]
    E_xz_i <- E_ss_i[xIndexes,zIndexes,drop=FALSE]
    
    E_z[[i]] <- E_z_i
    E_zz[[i]] <- E_zz_i
    E_x[[i]] <- E_x_i
    E_xx[[i]] <- E_xx_i
    E_xz[[i]] <- E_xz_i
  }
  
  list(
    E_x=E_x,
    E_xx=E_xx,
    E_z=E_z,
    E_zz=E_zz,
    E_xz=E_xz
  )
}
