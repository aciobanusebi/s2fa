#' Generate initial parameters for EM/MS3FA
#'
#' @param trainInput train input data; can contain NAs; error if a full row (input+output) is NA
#' @param trainOutput train output data; can contain NAs; error if a full row (input+output) is NA
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#'
#' @return initial parameters for EM/MS3FA, i.e. a list containing nDimX, nDimZ, type, mu_z_t, Sigma_z_t, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#'
#' @export
#' @examples
#' houseCopy <- house
#' house[1:10,2:3] <- NA
#' house[11:20,1] <- NA
#' params0 <- mS3faInit(trainInput=houseCopy[,2:3,drop=FALSE],
#'                      trainOutput=houseCopy[,1,drop=FALSE],
#'                      type="fa")
mS3faInit <- function(trainInput,trainOutput,type="fa") {
  nDimX <- ncol(trainInput)
  nDimZ <- ncol(trainOutput)
  mu_z <- convertToColumnVector(colMeans(trainOutput,na.rm = T))

  Sigma_z <- 0
  nCount <- 0
  for(i in 1:nrow(trainInput)) {
    x_i <- convertToColumnVector(trainInput[i,,drop=FALSE])
    z_i <- convertToColumnVector(trainOutput[i,,drop=FALSE])
    unsupervised <- getIsUnsupervised(z_i)
    if(!unsupervised) {
      Sigma_z <- Sigma_z + (z_i - mu_z) %*% t(z_i - mu_z)
      nCount <- nCount + 1
    }
  }
  Sigma_z <- Sigma_z / nCount

  if(nCount == 0) {
    mu_z <- convertToColumnVector(runif(nDimZ))
    Sigma_z <- matrix(nrow=nDimZ,ncol=nDimZ)
    for(i in 1:nDimZ) {
      for(j in i:nDimZ) {
        Sigma_z[i,j] <- runif(1)
        Sigma_z[j,i] <- Sigma_z[i,j]
      }
    }
  }

  mu <- convertToColumnVector(colMeans(trainInput,na.rm = T))
  psi <- 0
  nCount <- 0
  for(i in 1:nrow(trainInput)) {
    x_i <- convertToColumnVector(trainInput[i,,drop=FALSE])
    z_i <- convertToColumnVector(trainOutput[i,,drop=FALSE])
    unsupervised <- getIsUnsupervised(x_i)
    if(!unsupervised) {
      psi <- psi + (x_i - mu) %*% t(x_i - mu)
      nCount <- nCount + 1
    }
  }
  psi <- psi / nCount
  if(nCount == 0) {
    mu <- convertToColumnVector(runif(nDimX))
    psi <- matrix(nrow=nDimX,ncol=nDimX)
    for(i in 1:nDimX) {
      for(j in i:nDimX) {
        psi[i,j] <- runif(1)
        psi[j,i] <- psi[i,j]
      }
    }
  }

  # psi <- myCov(trainInput)
  if(type=="fa") {
    if(length(psi) != 1) {
      psi <- diag(diag(psi))
    }
  } else if(type=="ppca") {
    psi <- mean(diag(psi)) * diag(nrow(psi))
  }
  lambda <- matrix(runif(nDimX*nDimZ),nrow=nDimX,ncol=nDimZ)
  list(
    mu_z=mu_z,
    Sigma_z=Sigma_z,
    mu=mu,
    lambda=lambda,
    psi=psi
  )
}
