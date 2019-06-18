#' Generate initial parameters for EM/S3FA
#'
#' @param X_t_supervised train input data (which has output) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param Z_t_supervised train output data (for X_t_supervised) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param X_t_unsupervised train input data (which has no output) as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#'
#' @return initial parameters for EM/S3FA, i.e. a list containing nDimX, nDimZ, type, mu_z_t, Sigma_z_t, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#' @export
#'
#' @examples
#' params <- s3faInit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                    Z_t_supervised = house[1:10,1,drop=FALSE],
#'                    X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                    type = "fa",
#'                    checkArgs = TRUE)
#' params
s3faInit <- function(X_t_supervised, Z_t_supervised, X_t_unsupervised,
                     type = "fa",
                     checkArgs = TRUE) {

  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }

  if(checkArgs) {
    s3faInitCheckArgs(X_t_supervised, Z_t_supervised, X_t_unsupervised,type = type)
  }

  nDimX <- ncol(X_t_supervised)
  nDimZ <- ncol(Z_t_supervised)
  nTrain_supervised <- nrow(X_t_supervised)

  mu_z_t <- t(colMeans(Z_t_supervised)) # row vector

  if(nDimZ > nTrain_supervised) {
    Sigma_z_t <- genPositiveDefMat(nDimZ)$Sigma
  } else {
    Sigma_z_t <- t(Z_t_supervised) %*% Z_t_supervised - nTrain_supervised * t(mu_z_t) %*% mu_z_t
    Sigma_z_t <- Sigma_z_t / nTrain_supervised
  }

  aux <- rbind(X_t_supervised,
               X_t_unsupervised)
  mu_t <- t(colMeans(aux))
  psi_t <- myCov_t(aux,mu_t)

  if(type=="fa") {
    psi_t <- diag(psi_t)
  } else if(type=="ppca") {
    psi_t <- mean(diag(psi_t))
  }
  lambda_t <- matrix(runif(nDimX*nDimZ),nrow=nDimZ,ncol=nDimX)
  list(
    nDimX = nDimX,
    nDimZ = nDimZ,
    type = type,
    mu_z_t=mu_z_t,
    Sigma_z_t=Sigma_z_t,
    mu_t=mu_t,
    lambda_t=lambda_t,
    psi_t=psi_t
  )
}
