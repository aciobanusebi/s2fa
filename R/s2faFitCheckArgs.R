s2faFitCheckArgs <- function(X_t, 
                             Z_t, 
                             type, 
                             lambdaRidge) {
  type <- tolower(type)
  checkType(type)
  checkLambdaRidge(lambdaRidge)
  checkMatrix(X_t)
  checkMatrix(Z_t)
  checkMatrices(X_t,Z_t)
}