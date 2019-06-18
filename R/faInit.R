#' Generate initial parameters for EM/FA
#'
#' @param X_t_unsupervised train input data as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param nDimZ how many latent factors to consider
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#'
#' @return initial parameters for EM/FA, i.e. a list containing nDimX, nDimZ, type, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#' @export
#'
#' @examples
#' params <- faInit(X_t_unsupervised = house[,-1,drop=FALSE],
#'                  nDimZ=1,
#'                  type = "fa",checkArgs = TRUE)
#' params
faInit <- function(X_t_unsupervised,nDimZ,
                     type = "fa",
                     checkArgs = TRUE) {

  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }

  if(checkArgs) {
    faInitCheckArgs(X_t_unsupervised,nDimZ,type = type)
  }

  nDimX <- ncol(X_t_unsupervised)
  nTrain <- nrow(X_t_unsupervised)

  # mu_z_t <- t(runif(nDimZ)) # row vector
  # mu_z_t <- t(rep(0,nDimZ)) # row vector

  # Sigma_z_t <- genPositiveDefMat(nDimZ)$Sigma
  # Sigma_z_t <- diag(nDimZ)

  mu_t <- t(colMeans(X_t_unsupervised))
  psi_t <- myCov_t(X_t_unsupervised,mu_t)

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
    mu_t=mu_t,
    lambda_t=lambda_t,
    psi_t=psi_t
  )
}
