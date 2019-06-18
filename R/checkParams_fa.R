checkParams_fa <- function(p,checkPositiveDefinite=FALSE) {
  p$type <- checkType(p$type)
  if(!("matrix" %in% class(p$mu_t)) || any(is.na(p$mu_t))) {
    stop("p$mu_t not correspondingly set")
  }
  if(!("matrix" %in% class(p$lambda_t)) || any(is.na(p$lambda_t))) {
    stop("p$lambda_t not correspondingly set")
  }
  if(!("matrix" %in% class(p$psi_t) || is.numeric(p$psi_t)) || any(is.na(p$psi_t))) {
    stop("p$psi_t not correspondingly set")
  }
  if(!is.numeric(p$nDimX) ||  length(p$nDimX) != 1 || is.na(p$nDimX)) {
    stop("p$nDimX not correspondingly set")
  }
  if(!is.numeric(p$nDimZ) ||  length(p$nDimZ) != 1 || is.na(p$nDimZ)) {
    stop("p$nDimZ not correspondingly set")
  }
  nDimX <- p$nDimX
  if(!(p$type %in% c("ppca","fa"))) {
    if(nDimX != ncol(p$psi_t) ||
       nDimX != nrow(p$psi_t)) {
      stop("problem with number of dimensions in p")
    }
  } else if(p$type == "ppca") {
    if(1 != length(p$psi_t)) {
      stop("problem with number of dimensions in p")
    }
  } else {
    # fa
    if(nDimX != length(p$psi_t)) {
      stop("problem with number of dimensions in p")
    }
  }
  if(nDimX != ncol(p$mu_t) ||
     nDimX != ncol(p$lambda_t)) {
    stop("problem with number of dimensions in p")
  }
  nDimZ <- p$nDimZ
  if(nDimZ != nrow(p$lambda_t)) {
    stop("problem with number of latent dimensions in p")
  }
  if(checkPositiveDefinite) {
    if(!(p$type %in% c("ppca","fa"))) {
      # print(p$psi_t)
      if(!is.positive.definite(p$psi_t)) {
        stop("p$psi_t is not positive definite")
      }
    }
  }
}