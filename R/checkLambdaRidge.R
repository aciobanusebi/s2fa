checkLambdaRidge <- function(lambdaRidge) {
  if(!is.numeric(lambdaRidge) || length(lambdaRidge) != 1) {
    stop("lambdaRidge must be a number")
  }
}