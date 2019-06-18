#' Fit S2FA to data via analytic solution
#'
#' @param X_t train input data as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param Z_t train output data as design matrix (must be matrix, not data.frame), i.e. row = instance, column = feature/attribute
#' @param type "unconstrained", "fa", or "ppca"; refers to psi
#' @param lambdaRidge L2 regularization term; must be a number
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#'
#' @return final parameters learnt, i.e. a list containing nDimX, nDimZ, type, mu_z_,t Sigma_z_t, mu_t, lambda_t, psi_t. "_t" comes from "transpose"
#' @export
#'
#' @examples
#' params <- s2faFit(X_t=house[,2:3,drop=FALSE],
#'                   Z_t=house[,1,drop=FALSE],
#'                   type = "fa",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE)
#' params
s2faFit <- function(X_t, Z_t,
                    type = "fa",
                    lambdaRidge=0,
                    checkArgs=TRUE) {
  if(!is.logical(checkArgs)) {
    stop("checkArgs must be TRUE/FALSE")
  }
  if(checkArgs) {
    s2faFitCheckArgs(X_t, Z_t, type, lambdaRidge)
  }
  if(is.matrix(X_t) && nrow(X_t) == 1) {
    stop("cannot learn with only one instance")
  }

  type <- tolower(type)

  nTrain <- nrow(X_t)
  nDimX <- ncol(X_t)
  nDimZ <- ncol(Z_t)

  mu_z_t <- t(colMeans(Z_t)) # row vector
  Sigma_z_t <- myCov_t(Z_t,mu_z_t,withCorrection=FALSE)

  if(lambdaRidge == 0) {
    qrResult <- qr.solve(cbind(rep(1,nTrain),Z_t),
                         X_t)
    mu_t <- qrResult[1,,drop=FALSE]
    lambda_t <- qrResult[-1,,drop=FALSE]
  } else {
    mu_x_t <- t(colMeans(X_t)) # row vector
    aux <- try(solve(nTrain * crossprod(mu_z_t) - crossprod(Z_t) - lambdaRidge * diag(nDimZ)))
    if(class(aux) == "try-error") {
      stop("Try setting 'lambdaRidge' argument to a number != 0")
    }
    lambda_t <- aux %*%
      (nTrain * t(mu_z_t) %*% mu_x_t - t(Z_t) %*% X_t)
    mu_t <- mu_x_t - mu_z_t %*% lambda_t
  }

  psi_t <- crossprod(X_t - matrix(rep(1,nTrain)) %*% mu_t - Z_t %*% lambda_t) / nTrain

  if(type == "fa") {
    psi_t <- diag(psi_t)
  } else if(type == "ppca") {
    psi_t <- mean(diag(psi_t))
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
