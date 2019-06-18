myCov_t <- function(X_t,mu_t,withCorrection=FALSE) {
  n <- nrow(X_t)
  result <- crossprod(X_t - matrix(rep(1,n)) %*% mu_t) / n
  result
}

getLrModel <- function(trainOutput,trainInput) {
  lm(
    as.formula(paste(colnames(trainOutput), "~.",
                     sep = "")),
    data=cbind(trainInput,trainOutput)
  )
}

myLogNormal_t <- function(x_t,mu_t,Sigma_t) {
  mvtnorm::dmvnorm(x_t, mu_t, Sigma_t, log = TRUE)
}

#' For internal use
#'
#' @export
hasConvergedLogLike <- function(new,old,epsilon) {
  if(abs((new - old)/old) < epsilon) {
    cat("STOP:log-likelihoods equal\n")
    return(TRUE)
  }
  if(new > old) {
    cat("STOP:[numerical] error - newLogLike < oldLogLike\n")
    return(TRUE)
  }
  return(FALSE)
}

mySquaredNorm <- function(x) {
  sum(x^2)
}

#' For internal use
#'
#' @export
hasConvergedParameter <- function(new,old,epsilon) {
  if(mySquaredNorm(new - old)/mySquaredNorm(old) < epsilon) {
    cat("STOP:params equal\n")
    return(TRUE)
  }
  return(FALSE)
}

getParamsAsVector <- function(params) {
  params$nDimX <- NULL
  params$nDimZ <- NULL
  params$type <- NULL
  unlist(as.relistable(params))
}

getParamsFromVector <- function(array,type,nDimX,nDimZ,skeleton=NULL) {
  if(!is.null(skeleton)) {
    skeleton$nDimX <- NULL
    skeleton$nDimZ <- NULL
    skeleton$type <- NULL
    params <- relist(array,skeleton)
  } else {
    params <- relist(array)
  }
  params$nDimX <- nDimX
  params$nDimZ <- nDimZ
  params$type <- type
  params
}

sourceDirectory <- function(directory) {
  for(file in list.files(directory,full.names = T)) {
    source(file)
  }
}

getConditionalDistribution <- function(x,mu,Sigma,frontIndexes) {
  x_b <- x[-frontIndexes,,drop=FALSE]
  mu_a <- mu[frontIndexes,,drop=FALSE]
  mu_b <- mu[-frontIndexes,,drop=FALSE]
  Sigma_a <- Sigma[frontIndexes,frontIndexes,drop=FALSE]
  Sigma_b <- Sigma[-frontIndexes,-frontIndexes,drop=FALSE]
  Sigma_c <- Sigma[frontIndexes,-frontIndexes,drop=FALSE]

  mu_a_hat <- mu_a + Sigma_c %*% solve(Sigma_b) %*% (x_b - mu_b)
  Sigma_a_hat <- Sigma_a - Sigma_c %*% solve(Sigma_b) %*% t(Sigma_c)

  list(
    mu = mu_a_hat,
    Sigma = Sigma_a_hat
  )
}

getMarginalDistributionLogPdf <- function(x,mu,Sigma,indexes) {
  x_a <- x[indexes,,drop=FALSE]
  mu_a <- mu[indexes,,drop=FALSE]
  Sigma_a <- Sigma[indexes,indexes,drop=FALSE]
  myLogNormal(x_a,mu_a,Sigma_a)
}

myLogNormal <- function(x,mu,Sigma) {
  mvtnorm::dmvnorm(as.numeric(x), as.numeric(mu), Sigma, log = TRUE)
}

convertToColumnVector <- function(x) {
  matrix(as.numeric(x))
}

getIsUnsupervised <- function(x) {
  any(is.na(x))
}
