#' Predict new values, given the parameters of a trained S3FA model
#'
#' @param p parameters of a fitted S3FA
#' @param X_t_test a matrix containing the input data; their output values would be predicted
#' @param checkArgs whether to check the arguments are valid; it takes more time to execute
#' @param checkPositiveDefinite whether to check the covariance matrices are valid; it takes more time to execute
#'
#' @return a list containing the transposed predicted values ($values_t) and the covariance matrix associated with them ($Sigma_t)
#' @export
#'
#' @examples
#' params <- s3faFit(X_t_supervised = house[1:10,2:3,drop=FALSE],
#'                   Z_t_supervised = house[1:10,1,drop=FALSE],
#'                   X_t_unsupervised = house[11:20,2:3,drop=FALSE],
#'                   params=NULL,
#'                   type="fa",
#'                   lambdaRidge=0,
#'                   checkArgs=FALSE,
#'                   checkPositiveDefinite=FALSE,
#'                   epsilon=1e-10,
#'                   maxIterations=100,
#'                   stopType="objfn",
#'                   turboEmMethods=NULL)
#' result <- s3faPredict(p = params,
#'                       X_t_test = house[11:20,2:3,drop=FALSE],
#'                       checkArgs=TRUE,
#'                       checkPositiveDefinite=FALSE)
#' result
s3faPredict <- function(p,X_t_test,checkArgs=TRUE,checkPositiveDefinite=FALSE) {
  s2faPredict(p,X_t_test,checkArgs,checkPositiveDefinite)
}
